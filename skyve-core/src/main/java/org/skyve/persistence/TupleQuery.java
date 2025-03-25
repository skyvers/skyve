package org.skyve.persistence;

import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface TupleQuery {
	@Nonnull List<Object[]> tupleResults();
	@Nullable Object[] tupleResult();
	@Nonnull Object[] retrieveTuple();
	@Nonnull AutoClosingIterable<Object[]> tupleIterable();
}
