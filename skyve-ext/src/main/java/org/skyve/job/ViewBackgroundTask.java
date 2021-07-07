package org.skyve.job;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.job.AbstractSkyveJob;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.BackgroundTask;

/**
 * This is the default implementation of BackgroundTask integration and serves as the extension point.
 * @author mike
 *
 * @param <T>	The type of bean the task is operating on - usually the conversation bean.
 */
public abstract class ViewBackgroundTask<T extends Bean> implements BackgroundTask<T>, org.quartz.Job {
	private AbstractWebContext webContext;
	private T bean;
	private User user;
	
	/**
	 * Get the bean for the task.
	 */
	@Override
	public final T getBean() {
		return bean;
	}
	
	/**
	 * Place the conversation backing this task into the conversation cache.
	 */
	@Override
	public final void cacheConversation() throws Exception {
		StateUtil.cacheConversation(webContext);
	}
	
	/**
	 * Override this to do whatever is required in this task.
	 * @param bean	The same bean returned by getBean() for convenience.
	 */
	@Override
	public abstract void execute(@SuppressWarnings("hiding") T bean) throws Exception;
	
	/**
	 * Quartz integration point.
	 */
	@Override
	public final void execute(JobExecutionContext context) 
	throws JobExecutionException {
		AbstractPersistence persistence = null;
		try {
			JobDataMap map = context.getMergedJobDataMap();
			String webId = map.getString(AbstractWebContext.CONTEXT_NAME);
			user = (User) map.get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY);
        	webContext = StateUtil.getCachedConversation(webId, null, null);
			@SuppressWarnings("unchecked")
			T t = (T) webContext.getCurrentBean();
			bean = t;
        	
			persistence = webContext.getConversation();
            persistence.setForThread();
            persistence.setAsyncThread(true);
			persistence.begin();
			persistence.setUser(user);
			BeanProvider.injectFields(this);
			execute(bean);
		}
		catch (Throwable t) {
			Util.LOGGER.severe(getClass().getName() + " failed to execute - Exception caught : " + t.getLocalizedMessage());
		    t.printStackTrace();
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
