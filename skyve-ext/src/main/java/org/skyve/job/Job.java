package org.skyve.job;

import org.skyve.impl.job.AbstractSkyveJob;

public abstract class Job extends AbstractSkyveJob {
	private static final long serialVersionUID = 1333768405570850256L;

	/**
	 * Implement the job here.
	 */
	@Override
	public abstract void execute() throws Exception;

	/**
	 * Cancel the job here.
	 * If the job can't be cancelled then return a reason why the job cannot be cancelled, otherwise return null.
	 */
	@Override
	public abstract String cancel();
	
	private static final String EVENT_BUS_FACTORY = "org.primefaces.push.EventBusFactory";
	private static final String EVENT_BUS = "org.primefaces.push.EventBus";
	
	/**
	 * This method is used to push to all clients using atmosphere.
	 * @param path	The path (topic) that the message is intended for
	 * @param o	The object to push
	 */
	public static final void push(String path, Object o) throws Exception {
		// The below achieves - EventBusFactory.getDefault().eventBus().publish(path, o);
		// It uses the context class loader which is the web class loader even though this class
		// was loaded in the EJB class loader because primefaces and atmosphere libraries only 
		// work when placed in WEB-INF/lib.
    	Class<?> eventBusFactoryClass = Thread.currentThread().getContextClassLoader().loadClass(EVENT_BUS_FACTORY);
    	Object eventBusFactory = eventBusFactoryClass.getDeclaredMethod("getDefault").invoke(null);
    	Object eventBus = eventBusFactoryClass.getDeclaredMethod("eventBus").invoke(eventBusFactory);
    	Class<?> eventBusClass = Thread.currentThread().getContextClassLoader().loadClass(EVENT_BUS);
    	eventBusClass.getDeclaredMethod("publish", String.class, Object.class).invoke(eventBus, path, o);
	}
}
