package org.skyve.metadata.controller;

import org.skyve.domain.Bean;

/**
 * 
 */
public final class ServerSideActionResult<T extends Bean> {
	/**
	 * A reference to the bean sent in to the execute method. 
	 * The reference could have changed during processing - eg saving a new bean will yield a different reference.
	 */
	private T bean;

	/**
	 * 
	 * @param executeLocation
	 * @param bean
	 */
	public ServerSideActionResult(T bean) {
		this.bean = bean;
	}

	/**
	 * 
	 * @return
	 */
	public T getBean() {
		return bean;
	}

	/**
	 * 
	 * @param bean
	 */
	public void setBean(T bean) {
		this.bean = bean;
	}
}
