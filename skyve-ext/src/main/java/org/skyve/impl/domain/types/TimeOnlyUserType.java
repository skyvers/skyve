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

public class TimeOnlyUserType implements UserType, LiteralType<Date>, Serializable {
	private static final long serialVersionUID = 8170300540374728474L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.TIME};
	}

	@Override
	public Class<?> returnedClass() {
		return TimeOnly.class;
	}

	@Override
	public boolean equals(Object x, Object y) throws HibernateException {
		if (x == y) {
			return true;
		}
		if ((x == null) || (y == null)) {
			return false;
		}

		Date xdate = (Date) x;
		Date ydate = (Date) y;

		if (xdate.getTime() == ydate.getTime())
			return true;

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
	public int hashCode(Object o) 
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
	public Object nullSafeGet(ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
	throws HibernateException, SQLException {
		Time value = rs.getTime(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new TimeOnly(value.getTime());
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index, SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.TIME);
		}
		else {
			ps.setTime(index, new Time(((Date) value).getTime()));
		}
	}

	@Override
	public Object deepCopy(Object value)
	throws HibernateException {
		return (value == null) ? null : new TimeOnly(((Date) value).getTime());
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
		return (value == null) ? "NULL" : '\'' + new Time(value.getTime()).toString() + '\'';
	}
}
