package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;

public interface ProjectedQuery {
	public <T extends Bean> List<T> projectedResults() throws DomainException;
	public <T extends Bean> T projectedResult() throws DomainException;
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() throws DomainException;
}
