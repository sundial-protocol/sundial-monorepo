import { readFile } from 'node:fs/promises';
import os from 'node:os';
import { monitorEventLoopDelay } from 'node:perf_hooks';

const EVENT_LOOP_RESOLUTION_MS = 20;
const CPU_SATURATION_THRESHOLD_PERCENT = 90;
const MEMORY_SATURATION_THRESHOLD_RATIO = 0.8;
const EVENT_LOOP_LAG_SATURATION_THRESHOLD_MS = 100;
const NETWORK_IO_SATURATION_THRESHOLD_BYTES_PER_SEC = 100_000_000;

interface NetworkIoTotals {
  rxBytes: number | null;
  txBytes: number | null;
}

interface HostCpuTotals {
  totalMs: number;
  idleMs: number;
}

export interface HostResourceSnapshot {
  capturedAt: string;
  capturedAtMs: number;
  systemCpuTotalMs: number;
  systemCpuIdleMs: number;
  processCpuUserMicros: number;
  processCpuSystemMicros: number;
  processCpuTotalMicros: number;
  processRssBytes: number;
  processHeapUsedBytes: number;
  processHeapTotalBytes: number;
  systemTotalMemoryBytes: number;
  systemFreeMemoryBytes: number;
  eventLoopLagP95Ms: number | null;
  eventLoopLagMaxMs: number | null;
  eventLoopLagMeanMs: number | null;
  networkRxBytes: number | null;
  networkTxBytes: number | null;
}

export interface HostResourcePhaseEvidence {
  elapsedMs: number;
  hostCpuUtilizationPercent: number | null;
  processCpuPercent: number | null;
  processRssBytes: number;
  processRssDeltaBytes: number;
  processRssToSystemMemoryRatio: number | null;
  eventLoopLagP95Ms: number | null;
  eventLoopLagMaxMs: number | null;
  eventLoopLagMeanMs: number | null;
  networkRxDeltaBytes: number | null;
  networkTxDeltaBytes: number | null;
  networkRxBytesPerSec: number | null;
  networkTxBytesPerSec: number | null;
  networkTotalBytesPerSec: number | null;
}

export interface LoadDriverSaturationFlags {
  cpuSaturated: boolean | null;
  memorySaturated: boolean | null;
  eventLoopLagSaturated: boolean | null;
  networkIoSaturated: boolean | null;
  anySaturation: boolean;
  reasons: string[];
  thresholds: {
    cpuPercent: number;
    memoryRssToSystemRatio: number;
    eventLoopLagP95Ms: number;
    networkTotalBytesPerSec: number;
  };
}

export interface LoadDriverResourceEvidence {
  beforeLoad: HostResourceSnapshot;
  afterLoad: HostResourceSnapshot;
  afterRecovery: HostResourceSnapshot;
  loadPhase: HostResourcePhaseEvidence;
  recoveryPhase: HostResourcePhaseEvidence;
  totalPhase: HostResourcePhaseEvidence;
  saturationFlags: LoadDriverSaturationFlags;
}

function readHostCpuTotals(): HostCpuTotals {
  const cpus = os.cpus();
  let totalMs = 0;
  let idleMs = 0;

  for (const cpu of cpus) {
    const cpuTotal =
      cpu.times.user + cpu.times.nice + cpu.times.sys + cpu.times.idle + cpu.times.irq;
    totalMs += cpuTotal;
    idleMs += cpu.times.idle;
  }

  return { totalMs, idleMs };
}

async function readNetworkIoTotalsLinux(): Promise<NetworkIoTotals> {
  try {
    const raw = await readFile('/proc/net/dev', 'utf8');
    const lines = raw.split('\n').slice(2);
    let rxBytes = 0;
    let txBytes = 0;
    let seenDevice = false;

    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed.length === 0) {
        continue;
      }

      const [ifacePart, countersPart] = trimmed.split(':');
      if (ifacePart === undefined || countersPart === undefined) {
        continue;
      }

      const iface = ifacePart.trim();
      if (iface === 'lo') {
        continue;
      }

      const counters = countersPart.trim().split(/\s+/);
      if (counters.length < 9) {
        continue;
      }

      const rx = Number(counters[0]);
      const tx = Number(counters[8]);
      if (!Number.isFinite(rx) || !Number.isFinite(tx)) {
        continue;
      }

      rxBytes += rx;
      txBytes += tx;
      seenDevice = true;
    }

    if (!seenDevice) {
      return { rxBytes: null, txBytes: null };
    }

    return { rxBytes, txBytes };
  } catch {
    return { rxBytes: null, txBytes: null };
  }
}

function nanosToMs(ns: number): number {
  return ns / 1_000_000;
}

function maxNullable(values: Array<number | null>): number | null {
  const numbers = values.filter((value): value is number => value !== null);
  if (numbers.length === 0) {
    return null;
  }
  return Math.max(...numbers);
}

function computePhase(
  start: HostResourceSnapshot,
  end: HostResourceSnapshot
): HostResourcePhaseEvidence {
  const elapsedMs = Math.max(0, end.capturedAtMs - start.capturedAtMs);
  const elapsedSeconds = elapsedMs / 1000;

  const totalCpuDelta = end.systemCpuTotalMs - start.systemCpuTotalMs;
  const idleCpuDelta = end.systemCpuIdleMs - start.systemCpuIdleMs;
  const busyCpuDelta = totalCpuDelta - idleCpuDelta;
  const hostCpuUtilizationPercent = totalCpuDelta > 0 ? (busyCpuDelta / totalCpuDelta) * 100 : null;

  const processCpuDeltaMicros = end.processCpuTotalMicros - start.processCpuTotalMicros;
  const processCpuPercent =
    elapsedMs > 0 ? (processCpuDeltaMicros / (elapsedMs * 1000)) * 100 : null;

  const rssDeltaBytes = end.processRssBytes - start.processRssBytes;
  const processRssToSystemMemoryRatio =
    end.systemTotalMemoryBytes > 0 ? end.processRssBytes / end.systemTotalMemoryBytes : null;

  const networkRxDeltaBytes =
    start.networkRxBytes !== null && end.networkRxBytes !== null
      ? end.networkRxBytes - start.networkRxBytes
      : null;
  const networkTxDeltaBytes =
    start.networkTxBytes !== null && end.networkTxBytes !== null
      ? end.networkTxBytes - start.networkTxBytes
      : null;

  const networkRxBytesPerSec =
    networkRxDeltaBytes !== null && elapsedSeconds > 0
      ? networkRxDeltaBytes / elapsedSeconds
      : null;
  const networkTxBytesPerSec =
    networkTxDeltaBytes !== null && elapsedSeconds > 0
      ? networkTxDeltaBytes / elapsedSeconds
      : null;
  const networkTotalBytesPerSec =
    networkRxBytesPerSec !== null && networkTxBytesPerSec !== null
      ? networkRxBytesPerSec + networkTxBytesPerSec
      : null;

  return {
    elapsedMs,
    hostCpuUtilizationPercent,
    processCpuPercent,
    processRssBytes: end.processRssBytes,
    processRssDeltaBytes: rssDeltaBytes,
    processRssToSystemMemoryRatio,
    eventLoopLagP95Ms: end.eventLoopLagP95Ms,
    eventLoopLagMaxMs: end.eventLoopLagMaxMs,
    eventLoopLagMeanMs: end.eventLoopLagMeanMs,
    networkRxDeltaBytes,
    networkTxDeltaBytes,
    networkRxBytesPerSec,
    networkTxBytesPerSec,
    networkTotalBytesPerSec,
  };
}

function computeSaturationFlags(
  loadPhase: HostResourcePhaseEvidence,
  recoveryPhase: HostResourcePhaseEvidence,
  totalPhase: HostResourcePhaseEvidence
): LoadDriverSaturationFlags {
  const peakCpuPercent = maxNullable([
    loadPhase.hostCpuUtilizationPercent,
    recoveryPhase.hostCpuUtilizationPercent,
    totalPhase.hostCpuUtilizationPercent,
  ]);
  const peakMemoryRatio = maxNullable([
    loadPhase.processRssToSystemMemoryRatio,
    recoveryPhase.processRssToSystemMemoryRatio,
    totalPhase.processRssToSystemMemoryRatio,
  ]);
  const peakEventLoopLagP95Ms = maxNullable([
    loadPhase.eventLoopLagP95Ms,
    recoveryPhase.eventLoopLagP95Ms,
    totalPhase.eventLoopLagP95Ms,
  ]);
  const peakNetworkBytesPerSec = maxNullable([
    loadPhase.networkTotalBytesPerSec,
    recoveryPhase.networkTotalBytesPerSec,
    totalPhase.networkTotalBytesPerSec,
  ]);

  const cpuSaturated =
    peakCpuPercent !== null ? peakCpuPercent >= CPU_SATURATION_THRESHOLD_PERCENT : null;
  const memorySaturated =
    peakMemoryRatio !== null ? peakMemoryRatio >= MEMORY_SATURATION_THRESHOLD_RATIO : null;
  const eventLoopLagSaturated =
    peakEventLoopLagP95Ms !== null
      ? peakEventLoopLagP95Ms >= EVENT_LOOP_LAG_SATURATION_THRESHOLD_MS
      : null;
  const networkIoSaturated =
    peakNetworkBytesPerSec !== null
      ? peakNetworkBytesPerSec >= NETWORK_IO_SATURATION_THRESHOLD_BYTES_PER_SEC
      : null;

  const reasons: string[] = [];
  if (cpuSaturated === true) {
    reasons.push(
      `cpu >= ${CPU_SATURATION_THRESHOLD_PERCENT}% (observed ${peakCpuPercent?.toFixed(2)}%)`
    );
  }
  if (memorySaturated === true) {
    reasons.push(
      `rss/system memory >= ${(MEMORY_SATURATION_THRESHOLD_RATIO * 100).toFixed(0)}% (observed ${((peakMemoryRatio ?? 0) * 100).toFixed(2)}%)`
    );
  }
  if (eventLoopLagSaturated === true) {
    reasons.push(
      `event-loop lag p95 >= ${EVENT_LOOP_LAG_SATURATION_THRESHOLD_MS}ms (observed ${peakEventLoopLagP95Ms?.toFixed(2)}ms)`
    );
  }
  if (networkIoSaturated === true) {
    reasons.push(
      `network I/O >= ${NETWORK_IO_SATURATION_THRESHOLD_BYTES_PER_SEC} B/s (observed ${Math.round(peakNetworkBytesPerSec ?? 0)} B/s)`
    );
  }

  return {
    cpuSaturated,
    memorySaturated,
    eventLoopLagSaturated,
    networkIoSaturated,
    anySaturation:
      cpuSaturated === true ||
      memorySaturated === true ||
      eventLoopLagSaturated === true ||
      networkIoSaturated === true,
    reasons,
    thresholds: {
      cpuPercent: CPU_SATURATION_THRESHOLD_PERCENT,
      memoryRssToSystemRatio: MEMORY_SATURATION_THRESHOLD_RATIO,
      eventLoopLagP95Ms: EVENT_LOOP_LAG_SATURATION_THRESHOLD_MS,
      networkTotalBytesPerSec: NETWORK_IO_SATURATION_THRESHOLD_BYTES_PER_SEC,
    },
  };
}

export function buildLoadDriverResourceEvidence(
  beforeLoad: HostResourceSnapshot,
  afterLoad: HostResourceSnapshot,
  afterRecovery: HostResourceSnapshot
): LoadDriverResourceEvidence {
  const loadPhase = computePhase(beforeLoad, afterLoad);
  const recoveryPhase = computePhase(afterLoad, afterRecovery);
  const totalPhase = computePhase(beforeLoad, afterRecovery);
  return {
    beforeLoad,
    afterLoad,
    afterRecovery,
    loadPhase,
    recoveryPhase,
    totalPhase,
    saturationFlags: computeSaturationFlags(loadPhase, recoveryPhase, totalPhase),
  };
}

export interface HostResourceCollector {
  captureSnapshot(): Promise<HostResourceSnapshot>;
  resetEventLoopLag(): void;
  close(): void;
}

class DefaultHostResourceCollector implements HostResourceCollector {
  private readonly eventLoopHistogram = monitorEventLoopDelay({
    resolution: EVENT_LOOP_RESOLUTION_MS,
  });

  constructor() {
    this.eventLoopHistogram.enable();
    this.eventLoopHistogram.reset();
  }

  resetEventLoopLag(): void {
    this.eventLoopHistogram.reset();
  }

  async captureSnapshot(): Promise<HostResourceSnapshot> {
    const capturedAtMs = Date.now();
    const capturedAt = new Date(capturedAtMs).toISOString();
    const hostCpu = readHostCpuTotals();
    const processCpu = process.cpuUsage();
    const memory = process.memoryUsage();
    const networkIo = await readNetworkIoTotalsLinux();

    const eventLoopSamples = Number(this.eventLoopHistogram.count);
    const eventLoopLagP95Ms =
      eventLoopSamples > 0 ? nanosToMs(this.eventLoopHistogram.percentile(95)) : null;
    const eventLoopLagMaxMs = eventLoopSamples > 0 ? nanosToMs(this.eventLoopHistogram.max) : null;
    const eventLoopLagMeanMs =
      eventLoopSamples > 0 ? nanosToMs(this.eventLoopHistogram.mean) : null;

    return {
      capturedAt,
      capturedAtMs,
      systemCpuTotalMs: hostCpu.totalMs,
      systemCpuIdleMs: hostCpu.idleMs,
      processCpuUserMicros: processCpu.user,
      processCpuSystemMicros: processCpu.system,
      processCpuTotalMicros: processCpu.user + processCpu.system,
      processRssBytes: memory.rss,
      processHeapUsedBytes: memory.heapUsed,
      processHeapTotalBytes: memory.heapTotal,
      systemTotalMemoryBytes: os.totalmem(),
      systemFreeMemoryBytes: os.freemem(),
      eventLoopLagP95Ms,
      eventLoopLagMaxMs,
      eventLoopLagMeanMs,
      networkRxBytes: networkIo.rxBytes,
      networkTxBytes: networkIo.txBytes,
    };
  }

  close(): void {
    this.eventLoopHistogram.disable();
  }
}

export function createHostResourceCollector(): HostResourceCollector {
  return new DefaultHostResourceCollector();
}
