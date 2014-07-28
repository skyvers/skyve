package org.skyve.wildcat.domain.types;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;

public class OptimisticLockUserType implements UserType, Serializable, LiteralType {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 3237725890005410642L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.VARCHAR};
	}

	@Override
	public Class<?> returnedClass() {
		return OptimisticLock.class;
	}

	@Override
	public boolean equals(Object x, Object y)
	throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && ((OptimisticLock) x).equals(y));
	}

	@Override
	public int hashCode(Object o)
	throws HibernateException {
		return ((OptimisticLock) o).hashCode();
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, Object owner)
	throws HibernateException, SQLException {
		String value = rs.getString(names[0]);
		if ((rs.wasNull()) || (value == null) || (value.length() == 0)) {
			return null;
		}

		try {
			return new OptimisticLock(value);
		}
		catch (DomainException e) {
			throw new HibernateException("Could not create Update data from database value of " + value, e);
		}
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.VARCHAR);
		}
		else {
			ps.setString(index, ((OptimisticLock) value).toString());
		}
	}

	@Override
	public Object deepCopy(Object value)
	throws HibernateException {
		try {
			return (value == null) ? null : new OptimisticLock(value.toString());
		}
		catch (DomainException e) {
			throw new HibernateException("Could not copy OptimisticLock from database value of " + value, e);
		}
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
	public String objectToSQLString(Object value, Dialect dialect)
	throws Exception {
		return (value == null) ? "NULL" : value.toString();
	}
}
