package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface ProjectedQuery {
	@Nonnull <T extends Bean> List<T> projectedResults();
	@Nullable <T extends Bean> T projectedResult();
	@Nonnull <T extends Bean> T retrieveProjected();
	@Nonnull <T extends Bean> AutoClosingIterable<T> projectedIterable();
}
