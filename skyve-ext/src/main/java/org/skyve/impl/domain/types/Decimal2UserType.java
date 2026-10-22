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
import org.skyve.domain.types.Decimal2;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that maps the Skyve
 * {@link org.skyve.domain.types.Decimal2} type to a SQL {@code NUMERIC} column.
 */
public class Decimal2UserType implements UserType, Serializable, LiteralType<Number> {
	private static final long serialVersionUID = 7056962012110550357L;

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.NUMERIC};
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return Decimal2.class;
	}

	@Override
	public boolean equals(@Nullable Object x, @Nullable Object y) 
	throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && ((Decimal2) x).equals(y));
	}

	@Override
	public int hashCode(@SuppressWarnings("null") @Nonnull Object o) 
	throws HibernateException {
		return ((Decimal2) o).intValue();
	}

	@Override
	public @Nullable Object nullSafeGet(@SuppressWarnings("null") @Nonnull ResultSet rs,
										@SuppressWarnings("null") @Nonnull String[] names,
										@Nullable SharedSessionContractImplementor session,
										@Nullable Object owner)
	throws HibernateException, SQLException {
		BigDecimal value = rs.getBigDecimal(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new Decimal2(value);
	}

	@Override
	public void nullSafeSet(@SuppressWarnings("null") @Nonnull PreparedStatement ps,
							@Nullable Object value,
							int index,
							@Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.NUMERIC);
		}
		else if (value instanceof Decimal2 d) {
			ps.setBigDecimal(index, d.bigDecimalValue());
		}
		else if (value instanceof Decimal d) {
			ps.setBigDecimal(index, new Decimal2(d).bigDecimalValue());
		}
		else if (value instanceof BigDecimal d) {
			ps.setBigDecimal(index, new Decimal2(d).bigDecimalValue());
		}
		else {
			ps.setBigDecimal(index, new Decimal2(((Number) value).doubleValue()).bigDecimalValue());
		}
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value) 
	throws HibernateException {
		return (value == null) ? null : new Decimal2((Decimal) value);
	}

	@Override
	public boolean isMutable() {
		return false;
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
	public @Nonnull String objectToSQLString(@Nullable Number value, @Nullable Dialect dialect) 
	throws Exception {
		return (value == null) ? "NULL" : value.toString();
	}
}
