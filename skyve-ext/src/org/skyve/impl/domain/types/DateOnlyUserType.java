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

public class DateOnlyUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 4351232657679942727L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.DATE};
	}

	@Override
	public Class<?> returnedClass() {
		return DateOnly.class;
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
	public int hashCode(Object o) 
	throws HibernateException {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime((java.util.Date) o);
		return CalendarDateType.INSTANCE.getHashCode(calendar);
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
	throws HibernateException, SQLException {
		Date value = rs.getDate(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new DateOnly(value.getTime());
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index, SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.DATE);
		}
		else {
			ps.setDate(index, new Date(((DateOnly) value).getTime()));
		}
	}

	@Override
	public Object deepCopy(Object value) 
	throws HibernateException {
		return (value == null) ? null : new DateOnly(((java.util.Date) value).getTime());
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
		return (value == null) ? "NULL" : '\'' + new Date(value.getTime()).toString() + '\'';
	}
}
