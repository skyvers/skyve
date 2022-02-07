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
	private static final Logger LOGGER = LoggerFactory.getLogger(IteratingJob.class);

	@Inject
	private transient Persistence persistence;

	private int numProcessedElements;
	private int numSuccessfulElements;
	private int numFailedElements;
	private int numFlushedElements;
	private int numRolledBackElements;

	@Override
	@SuppressWarnings("boxing")
	public void execute() throws Exception {
		getLog().add(String.format("Commencing job %s.", getDisplayName()));

		final Collection<T> elementsToProcess = getElements();
		getLog().add(String.format("Found %d element(s) to process.", elementsToProcess.size()));
		numProcessedElements = 0;
		numSuccessfulElements = 0;
		numFailedElements = 0;
		numFlushedElements = 0;
		numRolledBackElements = 0;
		int numRolledBackElementsSinceLastCommit = 0;
		for (T element : elementsToProcess) {
			if (!isCancelled()) {
				try {
					operation(element);
					if (commitIfRequired(numProcessedElements + 1)) {
						incrementNumFlushedElements(numRolledBackElementsSinceLastCommit);
						numRolledBackElementsSinceLastCommit = 0;
					}
					incrementNumSuccessfulElements();
				} catch (final Exception e) {
					incrementNumFailedElements();
					if (!continueOnFailure()) {
						throw e;
					} else if (getCommitFrequency() != 0) {
						persistence.rollback();
						persistence.begin();
						if (getCommitFrequency() > 1) {
							numRolledBackElementsSinceLastCommit = (numProcessedElements + 1) - (numFlushedElements + numRolledBackElements);
							numRolledBackElements += numRolledBackElementsSinceLastCommit;
						} else {
							numRolledBackElements++;
						}
					}
					getLog().add(String.format("Exception processing element %d: %s", numProcessedElements, e.getMessage()));
					LOGGER.error("Exception processing element {}.", numProcessedElements, e);
				} finally {
					incrementNumProcessedElements();
				}
			} else {
				getLog().add(String.format("Job was cancelled after processing %d elements.", numProcessedElements));
				return;
			}
			setPercentComplete((int) (100.0 * numProcessedElements / elementsToProcess.size()));
		}

		setPercentComplete(100);

		// Check if there were any successful elements that were not flushed due to a rollback triggered by a processing failure.
		final int numUnflushedElements = numSuccessfulElements - numFlushedElements;
		if (getCommitFrequency() > 1 && numUnflushedElements > 0) {
			getLog().add(String.format("%d elements were not flushed to the database due to a rollback that was triggered by a processing failure. " +
							"You may want to change your commit frequency to 1 to ensure each successful element is flushed after it is processed.",
					numUnflushedElements));
		}

		getLog().add(String.format("Completing job %s. Successful: %d, Failed %d, Total: %d.", getDisplayName(),
				numSuccessfulElements, numFailedElements, numProcessedElements));

		// Throw an exception at the end if there were any failures so that the job gets marked as failed.
		if (numFailedElements > 0) {
			throw new RuntimeException(String.format("Failed to process %d elements.", numFailedElements));
		}
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
	@SuppressWarnings("static-method")
	protected int getCommitFrequency() {
		return 0;
	}

	/**
	 * Commits the current transaction if required.
	 *
	 * @param elementCount The number of elements that have been processed so far.
	 * @return True if a commit was performed.
	 */
	protected boolean commitIfRequired(int elementCount) {
		final int commitFrequency = getCommitFrequency();
		if (commitFrequency > 0 && elementCount % commitFrequency == 0) {
			persistence.commit(false);
			persistence.begin();
			return true;
		}
		return false;
	}

	/**
	 * Sets the persistence that this job will use if it is configured to perform partial commits.
	 *
	 * @param persistence Persistence to use for partial commits.
	 */
	protected void setPersistence(Persistence persistence) {
		this.persistence = persistence;
	}

	public int getNumProcessedElements() {
		return numProcessedElements;
	}

	protected void incrementNumProcessedElements() {
		numProcessedElements++;
	}

	public int getNumSuccessfulElements() {
		return numSuccessfulElements;
	}

	protected void incrementNumSuccessfulElements() {
		numSuccessfulElements++;
	}

	public int getNumFailedElements() {
		return numFailedElements;
	}

	protected void incrementNumFailedElements() {
		numFailedElements++;
	}

	/**
	 * @return The number of elements manually flushed by the job. Note that this will always be 0 if commit frequency is 0.
	 */
	public int getNumFlushedElements() {
		return numFlushedElements;
	}

	protected void incrementNumFlushedElements(int numElementsRolledBack) {
		numFlushedElements += getCommitFrequency() - numElementsRolledBack;
	}

	public int getNumRolledBackElements() {
		return numRolledBackElements;
	}
}
