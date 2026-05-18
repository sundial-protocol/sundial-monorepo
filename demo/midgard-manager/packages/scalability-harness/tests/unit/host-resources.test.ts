import { describe, expect, it } from 'vitest';

import type { HostResourceSnapshot } from '../../src/runner/host-resources.js';
import { buildLoadDriverResourceEvidence } from '../../src/runner/host-resources.js';

function makeSnapshot(overrides: Partial<HostResourceSnapshot>): HostResourceSnapshot {
  return {
    capturedAt: '2025-01-01T00:00:00.000Z',
    capturedAtMs: 0,
    systemCpuTotalMs: 10_000,
    systemCpuIdleMs: 7_000,
    processCpuUserMicros: 10_000,
    processCpuSystemMicros: 2_000,
    processCpuTotalMicros: 12_000,
    processRssBytes: 200_000_000,
    processHeapUsedBytes: 80_000_000,
    processHeapTotalBytes: 120_000_000,
    systemTotalMemoryBytes: 1_000_000_000,
    systemFreeMemoryBytes: 600_000_000,
    eventLoopLagP95Ms: 5,
    eventLoopLagMaxMs: 12,
    eventLoopLagMeanMs: 4,
    networkRxBytes: 1_000_000,
    networkTxBytes: 2_000_000,
    ...overrides,
  };
}

describe('buildLoadDriverResourceEvidence', () => {
  it('computes load/recovery/total phase evidence', () => {
    const before = makeSnapshot({
      capturedAtMs: 0,
      systemCpuTotalMs: 10_000,
      systemCpuIdleMs: 7_000,
      processCpuTotalMicros: 10_000,
      processRssBytes: 200_000_000,
      networkRxBytes: 1_000_000,
      networkTxBytes: 2_000_000,
    });
    const afterLoad = makeSnapshot({
      capturedAtMs: 60_000,
      systemCpuTotalMs: 14_000,
      systemCpuIdleMs: 7_200,
      processCpuTotalMicros: 5_500_000,
      processRssBytes: 820_000_000,
      eventLoopLagP95Ms: 120,
      networkRxBytes: 51_000_000,
      networkTxBytes: 70_000_000,
    });
    const afterRecovery = makeSnapshot({
      capturedAtMs: 90_000,
      systemCpuTotalMs: 16_000,
      systemCpuIdleMs: 8_200,
      processCpuTotalMicros: 6_100_000,
      processRssBytes: 790_000_000,
      eventLoopLagP95Ms: 70,
      networkRxBytes: 71_000_000,
      networkTxBytes: 90_000_000,
    });

    const evidence = buildLoadDriverResourceEvidence(before, afterLoad, afterRecovery);
    expect(evidence.loadPhase.elapsedMs).toBe(60_000);
    expect(evidence.recoveryPhase.elapsedMs).toBe(30_000);
    expect(evidence.totalPhase.elapsedMs).toBe(90_000);
    expect(evidence.loadPhase.processRssToSystemMemoryRatio).toBeCloseTo(0.82, 2);
    expect(evidence.loadPhase.hostCpuUtilizationPercent).toBeCloseTo(95, 0);
  });

  it('sets saturation flags when thresholds are exceeded', () => {
    const before = makeSnapshot({
      capturedAtMs: 0,
      processCpuTotalMicros: 0,
      processRssBytes: 200_000_000,
      systemCpuTotalMs: 10_000,
      systemCpuIdleMs: 9_000,
      networkRxBytes: 0,
      networkTxBytes: 0,
      eventLoopLagP95Ms: 1,
    });
    const afterLoad = makeSnapshot({
      capturedAtMs: 60_000,
      processCpuTotalMicros: 10_000_000,
      processRssBytes: 850_000_000,
      systemCpuTotalMs: 14_000,
      systemCpuIdleMs: 9_100,
      eventLoopLagP95Ms: 150,
      networkRxBytes: 5_000_000_000,
      networkTxBytes: 5_000_000_000,
    });
    const afterRecovery = makeSnapshot({
      capturedAtMs: 90_000,
      processCpuTotalMicros: 11_000_000,
      processRssBytes: 800_000_000,
      systemCpuTotalMs: 16_000,
      systemCpuIdleMs: 9_600,
      eventLoopLagP95Ms: 90,
      networkRxBytes: 6_000_000_000,
      networkTxBytes: 6_000_000_000,
    });

    const evidence = buildLoadDriverResourceEvidence(before, afterLoad, afterRecovery);
    expect(evidence.saturationFlags.cpuSaturated).toBe(true);
    expect(evidence.saturationFlags.memorySaturated).toBe(true);
    expect(evidence.saturationFlags.eventLoopLagSaturated).toBe(true);
    expect(evidence.saturationFlags.networkIoSaturated).toBe(true);
    expect(evidence.saturationFlags.anySaturation).toBe(true);
    expect(evidence.saturationFlags.reasons.length).toBeGreaterThan(0);
  });
});
