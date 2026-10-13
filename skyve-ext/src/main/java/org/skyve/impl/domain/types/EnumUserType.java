package org.skyve.impl.domain.types;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Properties;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.ParameterizedType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.types.Enumeration;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Hibernate {@link org.hibernate.usertype.UserType} that persists Skyve enum
 * attributes as their {@code code()} string value in a SQL {@code VARCHAR} column.
 */
public class EnumUserType implements UserType, Serializable, ParameterizedType {
	private static final long serialVersionUID = 8418711259919061318L;

	private Class<? extends Enum<?>> enumClass;
	private transient Method toCodeMethod;
	private transient Method fromCodeMethod;

	@Override
	@SuppressWarnings("unchecked")
	public void setParameterValues(@Nonnull Properties parameters) {
        String enumClassName = parameters.getProperty("enumClass");
        try {
            enumClass = (Class<? extends Enum<?>>) Thread.currentThread().getContextClassLoader().loadClass(enumClassName).asSubclass(Enum.class);
        }
        catch (ClassNotFoundException cfne) {
            throw new HibernateException("Enum class not found", cfne);
        }
        
        readResolve();
	}

	private @Nonnull Object readResolve() {
		try {
            toCodeMethod = enumClass.getMethod(Enumeration.TO_CODE_METHOD_NAME);
        }
        catch (Exception e) {
            throw new HibernateException("Failed to obtain toCode method", e);
        }

        try {
            fromCodeMethod = enumClass.getMethod(Enumeration.FROM_CODE_METHOD_NAME, String.class);
        }
        catch (Exception e) {
            throw new HibernateException("Failed to obtain fromCode method", e);
        }
        
        return this;
	}
	
	@Override
	public @Nullable Object assemble(@Nullable Serializable cached, @Nullable Object owner)
	throws HibernateException {
		return cached;
	}

	@Override
	public @Nullable Object deepCopy(@Nullable Object value)
	throws HibernateException {
		return value;
	}

	@Override
	public @Nullable Serializable disassemble(@Nullable Object value) throws HibernateException {
		return (Serializable) value;
	}

	@Override
	public boolean equals(@Nullable Object x, @Nullable Object y) throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && x.equals(y));
	}

	@Override
	public int hashCode(@Nonnull Object o) throws HibernateException {
		return o.hashCode();
	}

	@Override
	public boolean isMutable() {
		return false;
	}

	@Override
	public @Nullable Object nullSafeGet(@Nonnull ResultSet rs, @Nonnull String[] names, @Nullable SharedSessionContractImplementor session, @Nullable Object owner)
	throws HibernateException, SQLException {
        String code = rs.getString(names[0]);
		if ((rs.wasNull()) || (code == null) || code.isEmpty()) {
			return null;
		}
        
        try {
            return fromCodeMethod.invoke(enumClass, new Object[] {code});
        }
        catch (Exception e) {
            throw new HibernateException("Exception while invoking fromCode method '" + fromCodeMethod.getName() + "' of " +
                    						"enumeration class '" + enumClass + "'", e);
        }
	}

	@Override
	public void nullSafeSet(@Nonnull PreparedStatement ps, @Nullable Object value, int index, @Nullable SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
        try {
            if (value == null) {
            	ps.setNull(index, Types.VARCHAR);
            }
            else if (value instanceof String string) {
            	ps.setString(index, string);
            }
            else {
                String code = (String) toCodeMethod.invoke(value, new Object[0]);
                ps.setString(index, code);
            }
        }
        catch (Exception e) {
            throw new HibernateException("Exception while invoking toCode '" + toCodeMethod.getName() + "' of " +
                    						"enumeration class '" + enumClass + "'", e);
        }
	}

	@Override
	public @Nullable Object replace(@Nullable Object original, @Nullable Object target, @Nullable Object owner)
	throws HibernateException {
		return original;
	}

	@Override
	public @Nonnull Class<?> returnedClass() {
		return enumClass;
	}

	@Override
	public @Nonnull int[] sqlTypes() {
		return new int[] {Types.VARCHAR};
	}
}
