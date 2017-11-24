package org.skyve.persistence;

import java.util.List;

public interface ScalarQuery {
	public <T extends Object> List<T> scalarResults(Class<T> type);
	public <T extends Object> T scalarResult(Class<T> type);
	public <T extends Object> T retrieveScalar(Class<T> type);
	public <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type);
}
