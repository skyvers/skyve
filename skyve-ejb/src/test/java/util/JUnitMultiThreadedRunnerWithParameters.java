package util;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.parameterized.BlockJUnit4ClassRunnerWithParameters;
import org.junit.runners.parameterized.TestWithParameters;

/**
 * Runs each test on a separate thread in parallel with JUnit parameters and returns immediately.
 * The @AfterClass method of the test must monitor the thread count and when its zero, it'll exit.
 */
public class JUnitMultiThreadedRunnerWithParameters extends BlockJUnit4ClassRunnerWithParameters {
	public static AtomicInteger THREAD_COUNT = new AtomicInteger(0);

	public JUnitMultiThreadedRunnerWithParameters(TestWithParameters test) throws InitializationError {
		super(test);
	}

	@Override
	protected void runChild(final FrameworkMethod method, final RunNotifier notifier) {
		THREAD_COUNT.incrementAndGet();
		new Thread(new Test(method, notifier)).start();
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
			JUnitMultiThreadedRunnerWithParameters.super.runChild(method, notifier);
			THREAD_COUNT.decrementAndGet();
		}
	}
}
