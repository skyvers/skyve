package util;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;

/**
 * Runs each test on a separate thread in parallel and waits for them to complete.
 */
public class JUnitMultiThreadedRunner extends BlockJUnit4ClassRunner {
	private AtomicInteger threadCount;

	public JUnitMultiThreadedRunner(Class<?> klass) throws InitializationError {
		super(klass);
		threadCount = new AtomicInteger(0);
	}

	@Override
	protected void runChild(final FrameworkMethod method, final RunNotifier notifier) {
		threadCount.incrementAndGet();
		new Thread(new Test(method, notifier)).start();
	}

	@Override
	protected Statement childrenInvoker(final RunNotifier notifier) {
		return new Statement() {
			@Override
			@SuppressWarnings("synthetic-access")
			public void evaluate() throws Throwable {
				JUnitMultiThreadedRunner.super.childrenInvoker(notifier).evaluate();
				// wait for all threads to complete
				while (threadCount.get() > 0) {
					Thread.sleep(100);
				}
			}
		};
	}

	private class Test implements Runnable {
		private final FrameworkMethod method;
		private final RunNotifier notifier;

		public Test(FrameworkMethod method, RunNotifier notifier) {
			this.method = method;
			this.notifier = notifier;
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void run() {
			JUnitMultiThreadedRunner.super.runChild(method, notifier);
			threadCount.decrementAndGet();
		}
	}
}
