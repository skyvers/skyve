package util;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;

/**
 * Runs each test on a separate thread in parallel and waits for them to complete.
 */
public class JUnitMultiThreadedRunner extends BlockJUnit4ClassRunner {
	private final List<Thread> threads = new CopyOnWriteArrayList<>();

	public JUnitMultiThreadedRunner(Class<?> klass) throws InitializationError {
		super(klass);
	}

	@Override
	protected void runChild(final FrameworkMethod method, final RunNotifier notifier) {
		Thread thread = new Thread(new Test(method, notifier));
		threads.add(thread);
		thread.start();
	}

	@Override
	protected Statement childrenInvoker(final RunNotifier notifier) {
		return new Statement() {
			@Override
			@SuppressWarnings("synthetic-access")
			public void evaluate() throws Throwable {
				JUnitMultiThreadedRunner.super.childrenInvoker(notifier).evaluate();
				// wait for all threads to complete
				for (Thread thread : threads) {
					thread.join();
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
		}
	}
}
