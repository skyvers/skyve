package modules.admin.HeapDumpList.jobs;

import org.skyve.job.Job;

import modules.admin.HeapDumpList.util.HeapDumpUtil;

/**
 * Job to create a heap dump file nightly for memory analysis.
 * <p>
 * This job generates a timestamped heap dump in a customer-specific directory
 * under the content directory. It is intended to be scheduled to run
 * regularly (e.g., nightly) to help detect potential memory leaks by
 * capturing the JVM heap state over time.
 * <p>
 * <b>Note:</b> This method requires the <code>jdk.management</code> module to be
 * declared in <code>jboss-deployment-structure.xml</code>.
 */
public class DumpHeapJob extends Job {

	@Override
	public void execute() throws Exception {
		String dumpDir = HeapDumpUtil.dumpHeap();
		getLog().add("Heap dumped: " + dumpDir);

		setPercentComplete(100);
	}
}
