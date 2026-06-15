package org.skyve.metadata.controller;

import org.skyve.domain.Bean;

/**
 * Carries the result of a {@link ServerSideAction} execution back to the Skyve view pipeline.
 *
 * <p>The action places its (potentially updated or replaced) bean into this result object.
 * If the action replaces the bean — for example by saving a transient bean and obtaining
 * a new persistent reference — it must set the new reference via {@link #setBean} so that
 * Skyve can update the conversation state correctly.
 *
 * @param <T>  the document bean type
 * @see ServerSideAction
 */
public final class ServerSideActionResult<T extends Bean> {
	/**
	 * A reference to the bean sent in to the execute method. 
	 * The reference could have changed during processing - eg saving a new bean will yield a different reference.
	 */
	private T bean;

	/**
	 * Creates a result that carries the supplied bean back to the view.
	 *
	 * @param bean  the bean to return; may be a new instance if the action replaced the original
	 */
	public ServerSideActionResult(T bean) {
		this.bean = bean;
	}

	/**
	 * Returns the bean to use for the next render cycle.
	 *
	 * @return the bean; may differ from the bean passed into the action if it was replaced
	 */
	public T getBean() {
		return bean;
	}

	/**
	 * Replaces the bean that will be returned to the view.
	 *
	 * <p>Call this when the action has replaced the original bean, for example after
	 * persisting a new instance and obtaining its managed reference.
	 *
	 * @param bean  the replacement bean; must not be {@code null}
	 */
	public void setBean(T bean) {
		this.bean = bean;
	}
}
