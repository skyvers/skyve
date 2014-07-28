package org.skyve.metadata.controller;

import org.skyve.domain.Bean;

/**
 * 
 */
public final class ServerSideActionResult {
	/**
	 * A reference to the bean sent in to the execute method. 
	 * The reference could have changed during processing - eg saving a new bean will yield a different reference.
	 */
	private Bean bean;

	/**
	 * 
	 * @param executeLocation
	 * @param bean
	 */
	public ServerSideActionResult(Bean bean) {
		this.bean = bean;
	}

	/**
	 * 
	 * @return
	 */
	public Bean getBean() {
		return bean;
	}

	/**
	 * 
	 * @param bean
	 */
	public void setBean(Bean bean) {
		this.bean = bean;
	}
}
