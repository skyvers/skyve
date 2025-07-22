package org.skyve.persistence;

import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface ScalarQuery {
	@Nonnull <T extends Object> List<T> scalarResults(Class<T> type);
	@Nullable <T extends Object> T scalarResult(Class<T> type);
	@Nonnull <T extends Object> T retrieveScalar(Class<T> type);
	@Nonnull <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type);
}
