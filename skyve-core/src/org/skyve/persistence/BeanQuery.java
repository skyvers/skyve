package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

public interface BeanQuery {
	public <T extends Bean> List<T> beanResults();
	public <T extends Bean> T beanResult();
	public <T extends Bean> T retrieveBean();
	public <T extends Bean> AutoClosingIterable<T> beanIterable();
}
