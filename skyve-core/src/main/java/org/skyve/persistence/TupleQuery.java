package org.skyve.persistence;

import java.util.List;

public interface TupleQuery {
	public List<Object[]> tupleResults();
	public Object[] tupleResult();
	public Object[] retrieveTuple();
	public AutoClosingIterable<Object[]> tupleIterable();
}
