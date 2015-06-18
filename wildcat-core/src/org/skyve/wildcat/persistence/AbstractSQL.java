package org.skyve.wildcat.persistence;

import java.util.Date;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.persistence.SQL;

public abstract class AbstractSQL extends AbstractQuery implements SQL {
	private String query = null;

	public AbstractSQL(String query) {
		this.query = query;
	}

	@Override
	public AbstractSQL putParameter(String name, Object value) {
		if (value instanceof Decimal) {
			super.putParameter(name, ((Decimal) value).bigDecimalValue());
		}
		else if (value instanceof TimeOnly) {
			super.putParameter(name, new java.sql.Time(((Date) value).getTime()));
		}
		else if ((value instanceof Timestamp) || (value instanceof DateTime)) {
			super.putParameter(name, new java.sql.Timestamp(((Date) value).getTime()));
		}
		else if ((! (value instanceof java.sql.Date)) && (value instanceof Date)) {
			super.putParameter(name, new java.sql.Date(((Date) value).getTime()));
		}
		else {
			super.putParameter(name, value);
		}
		return this;
	}

	@Override
	public String toQueryString() {
		return query;
	}
	
	@Override
	public final <T extends Bean> T beanResult() throws DomainException {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) throws DomainException {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() throws DomainException {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}
}
