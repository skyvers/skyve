package modules.kitchensink.jobs;

import java.util.Collection;

import org.skyve.CORE;
import org.skyve.job.IteratingJob;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;

public class LongRunningJob extends IteratingJob<UserExtension> {

	private static final String THREAD_WAS_INTERRUPTED_EXITING_EARLY = "Thread was interrupted. Exiting early.";

	@Override
	public void execute() throws Exception {
		long totalDuration = 1 * 60 * 1000; // 5 minutes in milliseconds
		long startTime = System.currentTimeMillis();
		long endTime = startTime + totalDuration;

		getLog().add("Job started. Running for 5 minutes...");

		getElements().forEach(e -> {
			try {
				operation(e);
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		});

		while (System.currentTimeMillis() < endTime && !isCancelled()) {
			// You can put any work here, or leave it empty for a passive wait
			long elapsedTime = System.currentTimeMillis() - startTime;
			int newPercent = (int) ((elapsedTime * 100) / totalDuration);
			if (newPercent > getPercentComplete()) {
				setPercentComplete(newPercent);
			}
			try {
				Thread.sleep(1000); // Sleep for 1 second to avoid busy-waiting
			} catch (@SuppressWarnings("unused") InterruptedException e) {
				getLog().add(THREAD_WAS_INTERRUPTED_EXITING_EARLY);
				LOGGER.debug(THREAD_WAS_INTERRUPTED_EXITING_EARLY);
				break;
			}
		}
		if (isCancelled()) {
			getLog().add("Job Cancelled!");
			LOGGER.info("Job Cancelled!");
		} else {
			getLog().add("5 minutes elapsed. Job completed.");
		}

	}

	@Override
	protected Collection<UserExtension> getElements() {
		// select all users to update
		return CORE.getPersistence()
				.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME)
				.beanResults();
	}

	@Override
	protected void operation(UserExtension element) throws Exception {
		// perform an operation on each user
		getLog().add(element.getUserName());
	}

	@Override
	public String cancel() {
		LOGGER.info("Cancelling Job");
		return super.cancel();
	}

}
