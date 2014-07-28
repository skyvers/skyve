package org.skyve.wildcat.persistence;

import java.util.Date;

import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.persistence.SQL;

public class SQLImpl extends QueryImpl implements SQL {
	private String query = null;

	public SQLImpl(String query) {
		this.query = query;
	}

	@Override
	public void putParameter(String name, Object value) {
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
	}

	@Override
	public String toQueryString() {
		return query;
	}
}
