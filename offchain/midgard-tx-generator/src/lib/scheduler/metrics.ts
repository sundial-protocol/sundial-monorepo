/**
 * Simple metrics collector for basic telemetry
 */
export interface MetricsStats {
  submitted: number;
  failed: number;
  successRate: number;
  averageLatencyMs: number;
}

class Metrics {
  private submitted = 0;
  private failed = 0;
  private totalLatencyMs = 0;

  recordSubmission(success: boolean, count: number, latencyMs = 0) {
    if (success) {
      this.submitted += count;
      this.totalLatencyMs += latencyMs;
    } else {
      this.failed += count;
    }
  }

  getStats(): MetricsStats {
    const total = this.submitted + this.failed;
    return {
      submitted: this.submitted,
      failed: this.failed,
      successRate: total > 0 ? this.submitted / total : 0,
      averageLatencyMs:
        this.submitted > 0 ? this.totalLatencyMs / this.submitted : 0,
    };
  }

  reset() {
    this.submitted = 0;
    this.failed = 0;
    this.totalLatencyMs = 0;
  }
}

export const metrics = new Metrics();
