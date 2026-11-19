package org.skyve.job;

import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.job.AbstractSkyveJob;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.user.User;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.BackgroundTask;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * This is the default implementation of BackgroundTask integration and serves as the extension point.
 * @author mike
 *
 * @param <T>	The type of bean the task is operating on - usually the conversation bean.
 */
public abstract class ViewBackgroundTask<T extends Bean> implements BackgroundTask<T>, org.quartz.Job {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ViewBackgroundTask.class);

	private AbstractWebContext webContext;
	private T bean;

	/**
	 * Get the bean for the task.
	 */
	@Override
	public final @Nullable T getBean() {
		return bean;
	}
	
	/**
	 * Commit the transaction, cache the conversation backing this task and begin a new transaction.
	 */
	@Override
	public final void cacheConversationAndCycleTransaction() {
		StateUtil.commitAndCacheConversation(webContext);
		
		// The task may continue after its checkpoint in a new unit of work.
		AbstractPersistence persistence = webContext.getConversation();
		if (persistence != null) {
			persistence.begin();
		}
	}
	
	/**
	 * Quartz integration point.
	 */
	@Override
	public final void execute(@SuppressWarnings("null") @Nonnull JobExecutionContext context) 
	throws JobExecutionException {
		AbstractPersistence persistence = null;
		try {
			JobDataMap map = context.getMergedJobDataMap();
			String webId = map.getString(AbstractWebContext.CONTEXT_NAME);
			User user = (User) map.get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY);
			webContext = StateUtil.getCachedConversation(webId, null);
			@SuppressWarnings("unchecked")
			T t = (T) webContext.getNullableCurrentBean();
			bean = t;
        	
			persistence = webContext.getConversation();
            persistence.setForThread();
            persistence.setAsyncThread(true);
			persistence.begin();
			persistence.setUser(user);
			UtilImpl.inject(this);
			execute(bean);
		}
		catch (Throwable t) {
			LOGGER.error("{} failed to execute - Exception caught : {}", getClass().getName(), t.getLocalizedMessage(), t);
	    	if (persistence != null) {
	    		persistence.rollback();
	    	}
		}
		finally {
    	    // commit and close (its already been serialized to the conversations cache if needed)
    		if (persistence != null) {
    			persistence.commit(true);
    		}
		}
	}
}
