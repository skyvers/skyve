package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.messages.DomainException;

public interface ScalarQuery {
	public <T extends Object> List<T> scalarResults(Class<T> type) throws DomainException;
	public <T extends Object> T scalarResult(Class<T> type) throws DomainException;
	public <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type) throws DomainException;
}
