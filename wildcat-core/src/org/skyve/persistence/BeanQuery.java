package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;

public interface BeanQuery {
	public <T extends Bean> List<T> beanResults() throws DomainException;
	public <T extends Bean> T beanResult() throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> beanIterable() throws DomainException;
}
