package org.skyve.job;

import java.util.Collection;

import javax.inject.Inject;

import org.skyve.persistence.Persistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A job that performs an operation over a collection of elements.
 */
public abstract class IteratingJob<T> extends CancellableJob {
	private static final long serialVersionUID = 2364295456406284975L;

	private static final Logger LOGGER = LoggerFactory.getLogger(IteratingJob.class);

	@Inject
	private transient Persistence persistence;

	@Override
	public void execute() throws Exception {
		getLog().add(String.format("Commencing job %s.", getDisplayName()));

		final Collection<T> elementsToProcess = getElements();
		getLog().add(String.format("Found %d element(s) to process.", elementsToProcess.size()));
		int numProcessedElements = 0;
		int numSuccessfulElements = 0;
		int numFailedElements = 0;
		for (T element : elementsToProcess) {
			if (!isCancelled()) {
				try {
					operation(element);
					commitIfRequired(numProcessedElements);
					numSuccessfulElements++;
				} catch (final Exception e) {
					numFailedElements++;
					if (!continueOnFailure()) {
						throw e;
					}
					getLog().add(String.format("Exception processing element %d: %s", numProcessedElements, e.getMessage()));
					LOGGER.error("Exception processing element {}.", numProcessedElements, e);
				} finally {
					numProcessedElements++;
				}
			} else {
				getLog().add(String.format("Job was cancelled after processing %d elements.", numProcessedElements));
				return;
			}
			setPercentComplete((int) (100.0 * numProcessedElements / elementsToProcess.size()));
		}

		setPercentComplete(100);
		getLog().add(String.format("Completing job %s. Successful: %d, Failed %d, Total: %d.", getDisplayName(),
				numSuccessfulElements, numFailedElements, numProcessedElements));
	}

	/**
	 * @return The collection of elements to iterate over.
	 */
	protected abstract Collection<T> getElements();

	/**
	 * @param element The element to perform the operation on.
	 */
	protected abstract void operation(T element) throws Exception;

	/**
	 * @return Boolean that determines whether processing should continue if the operation fails.
	 */
	@SuppressWarnings("static-method")
	protected boolean continueOnFailure() {
		return false;
	}

	/**
	 * Allows partial commits during the job by controlling the number of operations to perform
	 * before committing the current transaction.
	 * <p>
	 * Return 0 to execute the job in one single transaction.
	 * Return 1 to commit after each operation.
	 * Return n (where n > 0) to commit after n operations.
	 *
	 * @return The number of operations to perform before committing.
	 */
	protected int getCommitFrequency() {
		return 0;
	}

	/**
	 * Commits the current transaction if required.
	 *
	 * @param numProcessedElements The number of elements that have been processed so far.
	 */
	protected void commitIfRequired(int numProcessedElements) {
		final int commitFrequency = getCommitFrequency();
		if (commitFrequency > 0 && numProcessedElements % commitFrequency == 0) {
			persistence.commit(false);
			persistence.begin();
		}
	}

	/**
	 * Sets the persistence that this job will use if it is configured to perform partial commits.
	 *
	 * @param persistence Persistence to use for partial commits.
	 */
	protected void setPersistence(Persistence persistence) {
		this.persistence = persistence;
	}
}
