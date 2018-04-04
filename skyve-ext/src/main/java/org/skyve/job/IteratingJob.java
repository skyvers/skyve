package org.skyve.job;

import java.util.Collection;

/**
 * A job that performs an operation over a collection of elements.
 */
public abstract class IteratingJob<T> extends CancellableJob {
    private static final long serialVersionUID = 2364295456406284975L;

    @Override
    public void execute() throws Exception {
        getLog().add(String.format("Commencing job %s.", getDisplayName()));

        final Collection<T> elementsToProcess = getElements();
        getLog().add(String.format("Found %d elements to process.", elementsToProcess.size()));
        int numProcessedElements = 0;
        for (T element : elementsToProcess) {
            if (!isCancelled()) {
                operation(element);
            } else {
                getLog().add(String.format("Job was cancelled after processing %d elements.", numProcessedElements));
                return;
            }
            setPercentComplete((int) (100.0 * numProcessedElements / elementsToProcess.size()));
        }

        setPercentComplete(100);
        getLog().add(String.format("Completing job %s.", getDisplayName()));
    }

    /**
     * @return The collection of elements to iterate over.
     */
    protected abstract Collection<T> getElements();

    /**
     * @param element The element to perform the operation on.
     */
    protected abstract void operation(T element);
}
