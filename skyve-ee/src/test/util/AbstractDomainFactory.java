package util;

import org.skyve.domain.Bean;

public abstract class AbstractDomainFactory<T extends Bean> {

	/**
	 * Return a new instance of this bean with all required scalar attributes
	 * and associations populated.
	 * 
	 * @return An instance of this bean, ready to be persisted for testing
	 * @throws Exception
	 */
	public abstract T getInstance() throws Exception;

}
