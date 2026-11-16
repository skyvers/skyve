package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Types;
import java.util.Calendar;
import java.util.Date;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.types.TimeOnly;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that maps the Skyve
 * {@link org.skyve.domain.types.TimeOnly} type to a SQL {@code TIME} column.
 */
public class TimeOnlyUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 8170300540374728474L;

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.TIME};
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return TimeOnly.class;
	}

	@Override
	public boolean equals(@Nullable Object x, @Nullable Object y) throws HibernateException {
		if (x == y) {
			return true;
		}
		if ((x == null) || (y == null)) {
			return false;
		}

		Date xdate = (Date) x;
		Date ydate = (Date) y;

		if (xdate.getTime() == ydate.getTime()) {
			return true;
		}
		
		Calendar calendar1 = Calendar.getInstance();
		Calendar calendar2 = Calendar.getInstance();
		calendar1.setTime(xdate);
		calendar2.setTime(ydate);

		return ((calendar1.get(Calendar.HOUR_OF_DAY) == calendar2.get(Calendar.HOUR_OF_DAY)) &&
					(calendar1.get(Calendar.MINUTE) == calendar2.get(Calendar.MINUTE)) &&
					(calendar1.get(Calendar.SECOND) == calendar2.get(Calendar.SECOND)) && 
					(calendar1.get(Calendar.MILLISECOND) == calendar2.get(Calendar.MILLISECOND)));
	}

	@Override
	public int hashCode(@SuppressWarnings("null") @Nonnull Object o) 
	throws HibernateException {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime((java.util.Date) o);
		int hashCode = 1;
		hashCode = 31 * hashCode + calendar.get(Calendar.HOUR_OF_DAY);
		hashCode = 31 * hashCode + calendar.get(Calendar.MINUTE);
		hashCode = 31 * hashCode + calendar.get(Calendar.SECOND);
		hashCode = 31 * hashCode + calendar.get(Calendar.MILLISECOND);

		return hashCode;
	}

	@Override
	public @Nullable Object nullSafeGet(@SuppressWarnings("null") @Nonnull ResultSet rs,
										@SuppressWarnings("null") @Nonnull String[] names,
										@Nullable SharedSessionContractImplementor session,
										@Nullable Object owner)
	throws HibernateException, SQLException {
		Time value = rs.getTime(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new TimeOnly(value.getTime());
	}

	@Override
	public void nullSafeSet(@SuppressWarnings("null") @Nonnull PreparedStatement ps,
							@Nullable Object value,
							int index,
							@Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.TIME);
		}
		else {
			ps.setTime(index, new Time(((Date) value).getTime()));
		}
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value)
	throws HibernateException {
		return (value == null) ? null : new TimeOnly(((Date) value).getTime());
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
		return (value == null) ? "NULL" : '\'' + new Time(value.getTime()).toString() + '\'';
	}
}
