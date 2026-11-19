package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Calendar;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.type.CalendarDateType;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.types.DateOnly;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that maps the Skyve
 * {@link org.skyve.domain.types.DateOnly} type to a SQL {@code DATE} column.
 */
public class DateOnlyUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 4351232657679942727L;

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.DATE};
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return DateOnly.class;
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
		
		java.util.Date xdate = (java.util.Date) x;
		java.util.Date ydate = (java.util.Date) y;

		if (xdate.getTime() == ydate.getTime()) {
			return true;
		}

		Calendar calendar1 = Calendar.getInstance();
		Calendar calendar2 = Calendar.getInstance();
		calendar1.setTime(xdate);
		calendar2.setTime(ydate);

		return CalendarDateType.INSTANCE.isEqual(calendar1, calendar2);
	}

	@Override
	public int hashCode(@SuppressWarnings("null") @Nonnull Object o) 
	throws HibernateException {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime((java.util.Date) o);
		return CalendarDateType.INSTANCE.getHashCode(calendar);
	}

	@Override
	public @Nullable Object nullSafeGet(@SuppressWarnings("null") @Nonnull ResultSet rs,
										@SuppressWarnings("null") @Nonnull String[] names,
										@Nullable SharedSessionContractImplementor session,
										@Nullable Object owner)
	throws HibernateException, SQLException {
		Date value = rs.getDate(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new DateOnly(value.getTime());
	}

	@Override
	public void nullSafeSet(@SuppressWarnings("null") @Nonnull PreparedStatement ps,
							@Nullable Object value,
							int index,
							@Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.DATE);
		}
		else {
			ps.setDate(index, new Date(((java.util.Date) value).getTime()));
		}
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value) 
	throws HibernateException {
		return (value == null) ? null : new DateOnly(((java.util.Date) value).getTime());
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
		return (value == null) ? "NULL" : '\'' + new Date(value.getTime()).toString() + '\'';
	}
}
