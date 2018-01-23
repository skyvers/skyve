package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Date;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;

public class TimestampUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 7498394614315784148L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.TIMESTAMP};
	}

	@Override
	public Class<?> returnedClass() {
		return org.skyve.domain.types.Timestamp.class;
	}

	@Override
	public boolean equals(Object x, Object y)
	throws HibernateException {
		if (x == y) {
			return true;
		}
		if ((x == null) || (y == null)) {
			return false;
		}

		long xTime = ((Date) x).getTime();
		long yTime = ((Date) y).getTime();
		boolean xts = (x instanceof Timestamp);
		boolean yts = (y instanceof Timestamp);
		int xNanos = xts ? ((Timestamp) x).getNanos() : 0;
		int yNanos = yts ? ((Timestamp) y).getNanos() : 0;
		if (xTime != yTime)
			return false;
		if (xts && yts) {
			// both are Timestamps
			int xn = xNanos % 1000000;
			int yn = yNanos % 1000000;
			return (xn == yn);
		}

		// at least one is a plain old Date
		return true;
	}

	@Override
	public int hashCode(Object o) 
	throws HibernateException {
		java.util.Date ts = (Date) o;
		return new Long(ts.getTime() / 1000).hashCode();
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
	throws HibernateException, SQLException {
		Timestamp value = rs.getTimestamp(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new org.skyve.domain.types.Timestamp(value.getTime());
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index, SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.TIMESTAMP);
		}
		else {
			ps.setTimestamp(index, new Timestamp(((Date) value).getTime()));
		}
	}

	@Override
	public Object deepCopy(Object value)
	throws HibernateException {
		return (value == null) ? null : new org.skyve.domain.types.Timestamp(((Date) value).getTime());
	}

	@Override
	public boolean isMutable() {
		return true;
	}

	@Override
	public Serializable disassemble(Object value)
	throws HibernateException {
		return (Serializable) value;
	}

	@Override
	public Object assemble(Serializable cached, Object owner)
	throws HibernateException {
		return cached;
	}

	@Override
	public Object replace(Object original, Object target, Object owner)
	throws HibernateException {
		return original;
	}

	@Override
	public String objectToSQLString(Date value, Dialect dialect)
	throws Exception {
		return (value == null) ? "NULL" : '\'' + new Timestamp(value.getTime()).toString() + '\'';
	}
}
