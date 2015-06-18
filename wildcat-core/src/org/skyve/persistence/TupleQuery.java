package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.messages.DomainException;

public interface TupleQuery {
	public List<Object[]> tupleResults() throws DomainException;
	public Object[] tupleResult() throws DomainException;
	public Object[] retrieveTuple() throws DomainException;
	public AutoClosingIterable<Object[]> tupleIterable() throws DomainException;
}
