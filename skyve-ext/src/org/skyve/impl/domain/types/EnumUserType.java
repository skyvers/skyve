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

public class EnumUserType implements UserType, Serializable, ParameterizedType {
	private static final long serialVersionUID = 8418711259919061318L;

	private Class<? extends Enum<?>> enumClass;
	private Method toCodeMethod;
	private Method fromCodeMethod;

	@Override
	@SuppressWarnings("unchecked")
	public void setParameterValues(Properties parameters) {
        String enumClassName = parameters.getProperty("enumClass");
        try {
            enumClass = (Class<? extends Enum<?>>) Thread.currentThread().getContextClassLoader().loadClass(enumClassName).asSubclass(Enum.class);
        }
        catch (ClassNotFoundException cfne) {
            throw new HibernateException("Enum class not found", cfne);
        }

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
	}

	@Override
	public Object assemble(Serializable cached, Object owner)
	throws HibernateException {
		return cached;
	}

	@Override
	public Object deepCopy(Object value)
	throws HibernateException {
		return value;
	}

	@Override
	public Serializable disassemble(Object value) throws HibernateException {
		return (Serializable) value;
	}

	@Override
	public boolean equals(Object x, Object y) throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && x.equals(y));
	}

	@Override
	public int hashCode(Object o) throws HibernateException {
		return o.hashCode();
	}

	@Override
	public boolean isMutable() {
		return false;
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, SharedSessionContractImplementor session, Object owner)
	throws HibernateException, SQLException {
        String code = rs.getString(names[0]);
		if ((rs.wasNull()) || (code == null) || (code.length() == 0)) {
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
	public void nullSafeSet(PreparedStatement ps, Object value, int index, SharedSessionContractImplementor session)
	throws HibernateException, SQLException {
        try {
            if (value == null) {
            	ps.setNull(index, Types.VARCHAR);
            }
            else if (value instanceof String) {
            	ps.setString(index, (String) value);
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
	public Object replace(Object original, Object target, Object owner)
	throws HibernateException {
		return original;
	}

	@Override
	public Class<?> returnedClass() {
		return enumClass;
	}

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.VARCHAR};
	}
}
