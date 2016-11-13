package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

public interface ProjectedQuery {
	public <T extends Bean> List<T> projectedResults();
	public <T extends Bean> T projectedResult();
	public <T extends Bean> T retrieveProjected();
	public <T extends Bean> AutoClosingIterable<T> projectedIterable();
}
