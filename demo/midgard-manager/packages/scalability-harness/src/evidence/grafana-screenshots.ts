import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

import type { StopConditions } from '../config/scenario.js';
import type { GrafanaScreenshotsConfig, ScalabilityScenario } from '../config/scenario.js';
import type { PrometheusSeries, TierMetricWindow } from '../metrics/window.js';
import type { StopConditionEvent } from './load-events.js';

const DASHBOARD_SLUG_FALLBACK = 'dashboard';
const DEFAULT_LOOKBACK_MINUTES = 10;
const DEFAULT_VIEWPORT_WIDTH = 1920;
const DEFAULT_VIEWPORT_HEIGHT = 1080;
const DEFAULT_WAIT_FOR_PANELS_MS = 4_000;
const DEFAULT_PEAK_CAPTURE_COOLDOWN_SECONDS = 60;
const DEFAULT_CAPTURE_PEAK_EVENTS = true;
const DEFAULT_CAPTURE_FINAL_PANEL_SET = true;
const DEFAULT_TIMEZONE = 'utc';
const DEFAULT_THEME = 'light';
const ONE_MINUTE_MS = 60_000;

const PEAK_METRICS = [
  'tx_queue_size',
  'mempool_tx_count',
  'commit_block_duration_seconds',
] as const;

interface DashboardPanelLayout {
  id: number;
  title: string;
  type: string;
  gridPos: { x: number; y: number; w: number; h: number } | null;
}

interface DashboardDescriptor {
  uid: string;
  title: string;
  slug: string;
  schemaVersion: number | null;
  panels: DashboardPanelLayout[];
}

export interface GrafanaScreenshotRecord {
  event: string;
  tierIndex?: number;
  metric?: string;
  stopReason?: StopConditionEvent['reason'];
  capturedAt: string;
  queryTimestamp: string;
  from: string;
  to: string;
  url: string;
  file: string;
  ok: boolean;
  error?: string;
}

interface GrafanaScreenshotManifest {
  runId: string;
  dashboard: {
    uid: string;
    title: string;
    schemaVersion: number | null;
    jsonPath: string;
  };
  config: {
    grafanaBaseUrl: string;
    timezone: string;
    theme: string;
    lookbackMinutes: number;
    viewportWidth: number;
    viewportHeight: number;
    waitForPanelsMs: number;
    peakCaptureCooldownSeconds: number;
    capturePeakEvents: boolean;
    captureFinalPanelSet: boolean;
  };
  captures: GrafanaScreenshotRecord[];
  panels: DashboardPanelLayout[];
}

interface BrowserLike {
  newPage(): Promise<PageLike>;
  close(): Promise<void>;
}

interface PageLike {
  setViewportSize(size: { width: number; height: number }): Promise<void>;
  goto(
    url: string,
    options?: { waitUntil?: 'load' | 'domcontentloaded' | 'networkidle'; timeout?: number }
  ): Promise<void>;
  waitForTimeout(timeoutMs: number): Promise<void>;
  screenshot(options: { path: string; fullPage?: boolean }): Promise<void>;
  close(): Promise<void>;
}

interface ChromiumLike {
  launch(options?: { headless?: boolean }): Promise<BrowserLike>;
}

interface ResolvedConfig {
  enabled: boolean;
  grafanaBaseUrl: string;
  dashboardJsonPath: string;
  dashboardUid?: string;
  timezone: string;
  theme: string;
  lookbackMinutes: number;
  viewportWidth: number;
  viewportHeight: number;
  waitForPanelsMs: number;
  peakCaptureCooldownSeconds: number;
  capturePeakEvents: boolean;
  captureFinalPanelSet: boolean;
}

function sanitizeSegment(value: string): string {
  const lower = value.toLowerCase();
  let out = '';
  let previousWasDash = false;

  for (let i = 0; i < lower.length; i += 1) {
    const code = lower.charCodeAt(i);
    const isDigit = code >= 48 && code <= 57;
    const isLowerAlpha = code >= 97 && code <= 122;

    if (isDigit || isLowerAlpha) {
      out += lower[i];
      previousWasDash = false;
      continue;
    }

    if (!previousWasDash && out.length > 0) {
      out += '-';
      previousWasDash = true;
    }
  }

  if (out.endsWith('-')) {
    out = out.slice(0, -1);
  }

  return out.slice(0, 80);
}

function resolveConfig(config: GrafanaScreenshotsConfig, scenarioPath: string): ResolvedConfig {
  const scenarioDir = path.dirname(scenarioPath);
  const dashboardJsonPath = path.isAbsolute(config.dashboardJsonPath)
    ? config.dashboardJsonPath
    : path.resolve(scenarioDir, config.dashboardJsonPath);

  return {
    enabled: config.enabled,
    grafanaBaseUrl: config.grafanaBaseUrl.replace(/\/$/, ''),
    dashboardJsonPath,
    dashboardUid: config.dashboardUid,
    timezone: config.timezone ?? DEFAULT_TIMEZONE,
    theme: config.theme ?? DEFAULT_THEME,
    lookbackMinutes: config.lookbackMinutes ?? DEFAULT_LOOKBACK_MINUTES,
    viewportWidth: config.viewportWidth ?? DEFAULT_VIEWPORT_WIDTH,
    viewportHeight: config.viewportHeight ?? DEFAULT_VIEWPORT_HEIGHT,
    waitForPanelsMs: config.waitForPanelsMs ?? DEFAULT_WAIT_FOR_PANELS_MS,
    peakCaptureCooldownSeconds:
      config.peakCaptureCooldownSeconds ?? DEFAULT_PEAK_CAPTURE_COOLDOWN_SECONDS,
    capturePeakEvents: config.capturePeakEvents ?? DEFAULT_CAPTURE_PEAK_EVENTS,
    captureFinalPanelSet: config.captureFinalPanelSet ?? DEFAULT_CAPTURE_FINAL_PANEL_SET,
  };
}

function flattenPanels(rawPanels: unknown[]): DashboardPanelLayout[] {
  const output: DashboardPanelLayout[] = [];

  const walk = (panels: unknown[]): void => {
    for (const panel of panels) {
      if (typeof panel !== 'object' || panel === null) continue;
      const p = panel as Record<string, unknown>;
      const id = typeof p.id === 'number' ? p.id : null;
      if (id !== null) {
        const gridPosRaw = p.gridPos;
        let gridPos: DashboardPanelLayout['gridPos'] = null;
        if (typeof gridPosRaw === 'object' && gridPosRaw !== null) {
          const gp = gridPosRaw as Record<string, unknown>;
          const x = typeof gp.x === 'number' ? gp.x : null;
          const y = typeof gp.y === 'number' ? gp.y : null;
          const w = typeof gp.w === 'number' ? gp.w : null;
          const h = typeof gp.h === 'number' ? gp.h : null;
          if (x !== null && y !== null && w !== null && h !== null) {
            gridPos = { x, y, w, h };
          }
        }

        output.push({
          id,
          title: typeof p.title === 'string' ? p.title : `panel-${id}`,
          type: typeof p.type === 'string' ? p.type : 'unknown',
          gridPos,
        });
      }

      if (Array.isArray(p.panels)) {
        walk(p.panels);
      }
    }
  };

  walk(rawPanels);
  output.sort((a, b) => {
    const ay = a.gridPos?.y ?? Number.MAX_SAFE_INTEGER;
    const by = b.gridPos?.y ?? Number.MAX_SAFE_INTEGER;
    if (ay !== by) return ay - by;
    const ax = a.gridPos?.x ?? Number.MAX_SAFE_INTEGER;
    const bx = b.gridPos?.x ?? Number.MAX_SAFE_INTEGER;
    if (ax !== bx) return ax - bx;
    return a.id - b.id;
  });
  return output;
}

async function parseDashboardDescriptor(
  dashboardJsonPath: string,
  dashboardUidOverride?: string
): Promise<DashboardDescriptor> {
  const raw = await readFile(dashboardJsonPath, 'utf8');
  const parsed = JSON.parse(raw) as Record<string, unknown>;
  const uidFromJson = typeof parsed.uid === 'string' && parsed.uid.length > 0 ? parsed.uid : null;
  const uid = dashboardUidOverride ?? uidFromJson;
  if (uid === undefined || uid === null || uid.length === 0) {
    throw new Error(`Dashboard uid missing in ${dashboardJsonPath} and no override provided.`);
  }

  const title =
    typeof parsed.title === 'string' && parsed.title.length > 0 ? parsed.title : 'dashboard';
  const slug = sanitizeSegment(title) || DASHBOARD_SLUG_FALLBACK;
  const schemaVersion = typeof parsed.schemaVersion === 'number' ? parsed.schemaVersion : null;
  const panels = Array.isArray(parsed.panels) ? flattenPanels(parsed.panels) : [];

  return { uid, title, slug, schemaVersion, panels };
}

function parseFinite(value: string): number | null {
  const numberValue = Number.parseFloat(value);
  return Number.isFinite(numberValue) ? numberValue : null;
}

function seriesToSummedPoints(series: PrometheusSeries[]): Array<{ tsMs: number; value: number }> {
  const sumsByTimestamp = new Map<number, number>();
  for (const row of series) {
    for (const [tsSec, rawValue] of row.values) {
      const parsed = parseFinite(rawValue);
      if (parsed === null) continue;
      const tsMs = Math.floor(tsSec * 1000);
      sumsByTimestamp.set(tsMs, (sumsByTimestamp.get(tsMs) ?? 0) + parsed);
    }
  }
  return [...sumsByTimestamp.entries()]
    .map(([tsMs, value]) => ({ tsMs, value }))
    .sort((a, b) => a.tsMs - b.tsMs);
}

function findFirstSeriesValueByPredicate(
  series: PrometheusSeries[] | undefined,
  predicate: (value: number) => boolean
): number | null {
  if (series === undefined) return null;
  const points = seriesToSummedPoints(series);
  for (const point of points) {
    if (predicate(point.value)) return point.tsMs;
  }
  return null;
}

async function loadChromium(): Promise<ChromiumLike> {
  const moduleName = 'playwright';
  const imported = (await import(moduleName)) as { chromium?: ChromiumLike };
  if (imported.chromium === undefined) {
    throw new Error('playwright.chromium is not available');
  }
  return imported.chromium;
}

export class GrafanaScreenshotService {
  static async createForScenario(
    scenario: ScalabilityScenario,
    scenarioPath: string,
    runDir: string
  ): Promise<GrafanaScreenshotService | null> {
    const cfg = scenario.grafanaScreenshots;
    if (cfg === undefined || !cfg.enabled) return null;

    const resolved = resolveConfig(cfg, scenarioPath);
    const descriptor = await parseDashboardDescriptor(
      resolved.dashboardJsonPath,
      resolved.dashboardUid
    );
    return new GrafanaScreenshotService(
      scenario.runId,
      runDir,
      resolved,
      descriptor,
      'grafana-screenshots',
      'grafana-screenshots.json'
    );
  }

  static async capturePlanSummary(
    scenario: ScalabilityScenario,
    scenarioPath: string,
    planDir: string,
    planId: string,
    fromIso: string,
    toIso: string
  ): Promise<void> {
    const cfg = scenario.grafanaScreenshots;
    if (cfg === undefined || !cfg.enabled) return;

    const resolved = resolveConfig(cfg, scenarioPath);
    const descriptor = await parseDashboardDescriptor(
      resolved.dashboardJsonPath,
      resolved.dashboardUid
    );

    const service = new GrafanaScreenshotService(
      planId,
      planDir,
      resolved,
      descriptor,
      path.join('grafana-screenshots', 'plan-summary'),
      'grafana-screenshots-plan-summary.json'
    );

    try {
      await service.captureLayoutAt('plan_summary_layout', toIso, undefined);
      if (resolved.captureFinalPanelSet) {
        await service.capturePanelSetAt('plan_summary', fromIso, toIso);
      }
    } finally {
      await service.close();
    }
  }

  private readonly outputDir: string;
  private readonly manifestPath: string;
  private readonly manifestRootDir: string;
  private readonly manifest: GrafanaScreenshotManifest;
  private browser: BrowserLike | null = null;
  private page: PageLike | null = null;
  private readonly peakValues: Record<string, number> = {};
  private lastPeakCaptureMs = Number.NEGATIVE_INFINITY;
  private stopThresholdCaptured = false;

  private constructor(
    runId: string,
    rootDir: string,
    private readonly config: ResolvedConfig,
    private readonly dashboard: DashboardDescriptor,
    screenshotsDirRelative: string,
    manifestFileName: string
  ) {
    this.manifestRootDir = rootDir;
    this.outputDir = path.join(rootDir, screenshotsDirRelative);
    this.manifestPath = path.join(rootDir, manifestFileName);
    this.manifest = {
      runId,
      dashboard: {
        uid: dashboard.uid,
        title: dashboard.title,
        schemaVersion: dashboard.schemaVersion,
        jsonPath: config.dashboardJsonPath,
      },
      config: {
        grafanaBaseUrl: config.grafanaBaseUrl,
        timezone: config.timezone,
        theme: config.theme,
        lookbackMinutes: config.lookbackMinutes,
        viewportWidth: config.viewportWidth,
        viewportHeight: config.viewportHeight,
        waitForPanelsMs: config.waitForPanelsMs,
        peakCaptureCooldownSeconds: config.peakCaptureCooldownSeconds,
        capturePeakEvents: config.capturePeakEvents,
        captureFinalPanelSet: config.captureFinalPanelSet,
      },
      captures: [],
      panels: dashboard.panels,
    };
  }

  async captureTierEvents(params: {
    tierIndex: number;
    startedAt: string;
    stoppedAt: string;
    recoveryStoppedAt: string;
    metricWindow: TierMetricWindow | null;
    metricStopReason: StopConditionEvent['reason'] | null;
    stopConditions: StopConditions;
  }): Promise<void> {
    await this.captureLayoutAt(
      `tier_${params.tierIndex}_baseline`,
      params.startedAt,
      params.tierIndex
    );
    await this.captureLayoutAt(
      `tier_${params.tierIndex}_after_load`,
      params.stoppedAt,
      params.tierIndex
    );
    await this.captureLayoutAt(
      `tier_${params.tierIndex}_after_recovery`,
      params.recoveryStoppedAt,
      params.tierIndex
    );

    if (
      params.metricStopReason !== null &&
      params.metricWindow !== null &&
      !this.stopThresholdCaptured
    ) {
      const stopTs = this.findStopThresholdCrossingTime(
        params.metricStopReason,
        params.metricWindow,
        params.stopConditions
      );
      const stopIso = new Date(stopTs).toISOString();
      await this.captureLayoutAt(
        `tier_${params.tierIndex}_stop_threshold_crossed`,
        stopIso,
        params.tierIndex,
        undefined,
        params.metricStopReason
      );
      this.stopThresholdCaptured = true;
    }

    if (params.metricWindow !== null && this.config.capturePeakEvents) {
      await this.capturePeakEvents(params.tierIndex, params.metricWindow);
    }
  }

  async close(): Promise<void> {
    await this.persistManifest();
    await this.page?.close();
    await this.browser?.close();
    this.page = null;
    this.browser = null;
  }

  private async ensureBrowser(): Promise<void> {
    if (this.browser !== null && this.page !== null) return;
    await mkdir(this.outputDir, { recursive: true });
    const chromium = await loadChromium();
    this.browser = await chromium.launch({ headless: true });
    this.page = await this.browser.newPage();
    await this.page.setViewportSize({
      width: this.config.viewportWidth,
      height: this.config.viewportHeight,
    });
  }

  private buildDashboardUrl(fromMs: number, toMs: number, panelId?: number): string {
    const pathPrefix = panelId === undefined ? 'd' : 'd-solo';
    const params = new URLSearchParams({
      from: String(fromMs),
      to: String(toMs),
      timezone: this.config.timezone,
      theme: this.config.theme,
      kiosk: '',
    });
    if (panelId !== undefined) {
      params.set('panelId', String(panelId));
    }
    return `${this.config.grafanaBaseUrl}/${pathPrefix}/${this.dashboard.uid}/${this.dashboard.slug}?${params.toString()}`;
  }

  private async captureLayoutAt(
    event: string,
    queryTimestampIso: string,
    tierIndex?: number,
    metric?: string,
    stopReason?: StopConditionEvent['reason']
  ): Promise<void> {
    const queryTsMs = new Date(queryTimestampIso).getTime();
    const lookbackMs = this.config.lookbackMinutes * ONE_MINUTE_MS;
    const fromMs = Math.max(0, queryTsMs - lookbackMs);
    const toMs = queryTsMs + ONE_MINUTE_MS;
    const captureLabel =
      `${sanitizeSegment(event)}-${new Date(queryTsMs).toISOString().replace(/[:.]/g, '-')}` +
      '.png';
    const screenshotPath = path.join(this.outputDir, captureLabel);
    const fileRelative = path.relative(this.manifestRootDir, screenshotPath);
    const url = this.buildDashboardUrl(fromMs, toMs);
    const capturedAt = new Date().toISOString();

    try {
      await this.ensureBrowser();
      if (this.page === null) throw new Error('playwright page not initialized');
      await this.page.goto(url, { waitUntil: 'networkidle', timeout: 60_000 });
      await this.page.waitForTimeout(this.config.waitForPanelsMs);
      await this.page.screenshot({ path: screenshotPath, fullPage: true });
      this.manifest.captures.push({
        event,
        tierIndex,
        metric,
        stopReason,
        capturedAt,
        queryTimestamp: queryTimestampIso,
        from: new Date(fromMs).toISOString(),
        to: new Date(toMs).toISOString(),
        url,
        file: fileRelative,
        ok: true,
      });
    } catch (error) {
      this.manifest.captures.push({
        event,
        tierIndex,
        metric,
        stopReason,
        capturedAt,
        queryTimestamp: queryTimestampIso,
        from: new Date(fromMs).toISOString(),
        to: new Date(toMs).toISOString(),
        url,
        file: fileRelative,
        ok: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
    await this.persistManifest();
  }

  private async capturePanelSetAt(
    eventPrefix: string,
    fromIso: string,
    toIso: string
  ): Promise<void> {
    const fromMs = new Date(fromIso).getTime();
    const toMs = new Date(toIso).getTime();
    for (const panel of this.dashboard.panels) {
      const panelEvent = `${eventPrefix}_panel_${panel.id}`;
      const captureLabel = `${sanitizeSegment(panelEvent)}-${new Date(toMs).toISOString().replace(/[:.]/g, '-')}.png`;
      const screenshotPath = path.join(this.outputDir, captureLabel);
      const fileRelative = path.relative(this.manifestRootDir, screenshotPath);
      const url = this.buildDashboardUrl(fromMs, toMs, panel.id);
      const capturedAt = new Date().toISOString();
      try {
        await this.ensureBrowser();
        if (this.page === null) throw new Error('playwright page not initialized');
        await this.page.goto(url, { waitUntil: 'networkidle', timeout: 60_000 });
        await this.page.waitForTimeout(this.config.waitForPanelsMs);
        await this.page.screenshot({ path: screenshotPath, fullPage: true });
        this.manifest.captures.push({
          event: panelEvent,
          capturedAt,
          queryTimestamp: toIso,
          from: new Date(fromMs).toISOString(),
          to: new Date(toMs).toISOString(),
          url,
          file: fileRelative,
          ok: true,
        });
      } catch (error) {
        this.manifest.captures.push({
          event: panelEvent,
          capturedAt,
          queryTimestamp: toIso,
          from: new Date(fromMs).toISOString(),
          to: new Date(toMs).toISOString(),
          url,
          file: fileRelative,
          ok: false,
          error: error instanceof Error ? error.message : String(error),
        });
      }
    }
    await this.persistManifest();
  }

  private findStopThresholdCrossingTime(
    reason: StopConditionEvent['reason'],
    window: TierMetricWindow,
    stopConditions: StopConditions
  ): number {
    const fallback = new Date(window.stoppedAt).getTime();

    if (reason === 'prometheus_down') {
      return (
        findFirstSeriesValueByPredicate(
          window.ranges['up{job="midgard_nodes"}'],
          (value) => value <= 0
        ) ?? fallback
      );
    }

    if (reason === 'commitment_failure') {
      const baseline = window.before['commit_block_commitment_failures_total'] ?? 0;
      return (
        findFirstSeriesValueByPredicate(
          window.ranges['commit_block_commitment_failures_total'],
          (value) => value > baseline
        ) ?? fallback
      );
    }

    if (reason === 'merge_failure') {
      const baseline = window.before['merge_block_failures_total'] ?? 0;
      return (
        findFirstSeriesValueByPredicate(
          window.ranges['merge_block_failures_total'],
          (value) => value > baseline
        ) ?? fallback
      );
    }

    if (reason === 'recovery_queue_exceeded') {
      if (stopConditions.maxRecoveryQueueSize === undefined) return fallback;
      return (
        findFirstSeriesValueByPredicate(
          window.ranges['tx_queue_size'],
          (value) => value > stopConditions.maxRecoveryQueueSize!
        ) ?? fallback
      );
    }

    if (reason === 'recovery_mempool_exceeded') {
      if (stopConditions.maxRecoveryMempoolSize === undefined) return fallback;
      return (
        findFirstSeriesValueByPredicate(
          window.ranges['mempool_tx_count'],
          (value) => value > stopConditions.maxRecoveryMempoolSize!
        ) ?? fallback
      );
    }

    return fallback;
  }

  private async capturePeakEvents(tierIndex: number, window: TierMetricWindow): Promise<void> {
    const cooldownMs = this.config.peakCaptureCooldownSeconds * 1000;

    for (const metric of PEAK_METRICS) {
      const points = seriesToSummedPoints(window.ranges[metric] ?? []);
      if (points.length === 0) continue;

      let metricPeak = this.peakValues[metric] ?? Number.NEGATIVE_INFINITY;
      for (const point of points) {
        if (point.value <= metricPeak) continue;
        metricPeak = point.value;
        this.peakValues[metric] = metricPeak;
        if (point.tsMs - this.lastPeakCaptureMs < cooldownMs) continue;

        this.lastPeakCaptureMs = point.tsMs;
        await this.captureLayoutAt(
          `tier_${tierIndex}_peak_${metric}`,
          new Date(point.tsMs).toISOString(),
          tierIndex,
          metric
        );
      }
    }
  }

  private async persistManifest(): Promise<void> {
    await writeFile(this.manifestPath, JSON.stringify(this.manifest, null, 2));
  }
}
