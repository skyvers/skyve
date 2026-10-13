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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that maps the Skyve
 * {@link org.skyve.domain.types.Timestamp} type to a SQL {@code TIMESTAMP} column.
 */
public class TimestampUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 7498394614315784148L;

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.TIMESTAMP};
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return org.skyve.domain.types.Timestamp.class;
	}

	@Override
	public boolean equals(@Nullable Object x, @Nullable Object y)
	throws HibernateException {
		if (x == y) {
			return true;
		}
		if ((x == null) || (y == null)) {
			return false;
		}

		long xTime = ((Date) x).getTime();
		long yTime = ((Date) y).getTime();
		if (xTime != yTime) {
			return false;
		}

		boolean xts = (x instanceof Timestamp);
		boolean yts = (y instanceof Timestamp);
		int xNanos = xts ? ((Timestamp) x).getNanos() : 0;
		int yNanos = yts ? ((Timestamp) y).getNanos() : 0;
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
	public int hashCode(@Nonnull Object o) 
	throws HibernateException {
		java.util.Date ts = (Date) o;
		return Long.valueOf(ts.getTime() / 1000).hashCode();
	}

	@Override
	public @Nullable Object nullSafeGet(@Nonnull ResultSet rs, @Nonnull String[] names, @Nullable SharedSessionContractImplementor session, @Nullable Object owner)
	throws HibernateException, SQLException {
		Timestamp value = rs.getTimestamp(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new org.skyve.domain.types.Timestamp(value.getTime());
	}

	@Override
	public void nullSafeSet(@Nonnull PreparedStatement ps, @Nullable Object value, int index, @Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.TIMESTAMP);
		}
		else {
			ps.setTimestamp(index, new Timestamp(((Date) value).getTime()));
		}
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value)
	throws HibernateException {
		return (value == null) ? null : new org.skyve.domain.types.Timestamp(((Date) value).getTime());
	}

	@Override
	public boolean isMutable() {
		return true;
	}

	@Override
	public @Nullable Serializable disassemble(@Nullable Object value)
	throws HibernateException {
		return (Serializable) value;
	}

	@Override
	public @Nullable Object assemble(@Nullable Serializable cached, @Nullable Object owner)
	throws HibernateException {
		return cached;
	}

	@Override
	public @Nullable Object replace(@Nullable Object original, @Nullable Object target, @Nullable Object owner)
	throws HibernateException {
		return original;
	}

	@Override
	public @Nonnull String objectToSQLString(@Nullable Date value, @Nullable Dialect dialect)
	throws Exception {
		return (value == null) ? "NULL" : '\'' + new Timestamp(value.getTime()).toString() + '\'';
	}
}
