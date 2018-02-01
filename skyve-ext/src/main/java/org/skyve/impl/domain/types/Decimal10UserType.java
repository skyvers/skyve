package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;

public class Decimal10UserType implements UserType, Serializable, LiteralType<Number> {
	private static final long serialVersionUID = 189859600281114825L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.NUMERIC};
	}

	@Override
	public Class<?> returnedClass() {
		return Decimal10.class;
	}

	@Override
	public boolean equals(Object x, Object y) 
	throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && ((Decimal10) x).equals(y));
	}

	@Override
	public int hashCode(Object o) 
	throws HibernateException {
		return ((Decimal10) o).intValue();
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
	throws HibernateException, SQLException {
		BigDecimal value = rs.getBigDecimal(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new Decimal10(value);
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index, SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.NUMERIC);
		}
		else if (value instanceof Decimal10) {
			ps.setBigDecimal(index, ((Decimal10) value).bigDecimalValue());
		}
		else if (value instanceof Decimal) {
			ps.setBigDecimal(index, new Decimal10((Decimal) value).bigDecimalValue());
		}
		else if (value instanceof BigDecimal) {
			ps.setBigDecimal(index, new Decimal10((BigDecimal) value).bigDecimalValue());
		}
		else {
			ps.setBigDecimal(index, new Decimal10(((Number) value).doubleValue()).bigDecimalValue());
		}
	}

	@Override
	public Object deepCopy(Object value) 
	throws HibernateException {
		return (value == null) ? null : new Decimal10((Decimal) value);
	}

	@Override
	public boolean isMutable() {
		return false;
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
	public String objectToSQLString(Number value, Dialect dialect) 
	throws Exception {
		return (value == null) ? "NULL" : value.toString();
	}
}
