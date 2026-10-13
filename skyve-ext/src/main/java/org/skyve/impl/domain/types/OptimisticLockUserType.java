package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that maps a Skyve
 * {@link org.skyve.domain.types.OptimisticLock} composite value to its
 * serialised string representation in the database.
 */
public class OptimisticLockUserType implements UserType, Serializable, LiteralType<OptimisticLock> {
	private static final long serialVersionUID = 3237725890005410642L;

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.VARCHAR};
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return OptimisticLock.class;
	}

	@Override
	public boolean equals(@Nullable Object x, @Nullable Object y)
	throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && ((OptimisticLock) x).equals(y));
	}

	@Override
	public int hashCode(@Nonnull Object o)
	throws HibernateException {
		return ((OptimisticLock) o).hashCode();
	}

	@Override
	public @Nullable Object nullSafeGet(@Nonnull ResultSet rs, @Nonnull String[] names, @Nullable SharedSessionContractImplementor session, @Nullable Object owner)
	throws HibernateException, SQLException {
		String value = rs.getString(names[0]);
		if (rs.wasNull() || (value == null) || value.isEmpty()) {
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
	public void nullSafeSet(@Nonnull PreparedStatement ps, @Nullable Object value, int index, @Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.VARCHAR);
		}
		else {
			ps.setString(index, ((OptimisticLock) value).toString());
		}
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value)
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
	public @Nonnull String objectToSQLString(@Nullable OptimisticLock value, @Nullable Dialect dialect)
	throws Exception {
		return (value == null) ? "NULL" : value.toString();
	}
}
