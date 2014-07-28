package org.skyve.wildcat.util;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.BasicDynaBean;
import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.DynaClass;
import org.apache.commons.beanutils.DynaProperty;
import org.apache.commons.beanutils.RowSetDynaClass;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.wildcat.bind.BindUtil;

/**
 * Note that a lot of the state of this class is transient as the DynaBean 
 * implementation keeps a reference of the class that created it.
 */
final class SQLRowSetDynaClass implements DynaClass, Serializable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 3023493876835105207L;

	/**
	 * The set of dynamic properties that are part of this {@link DynaClass}.
	 */
	protected DynaProperty properties[] = null;

	/**
	 * The set of dynamic properties that are part of this {@link DynaClass}, keyed by the property name. 
	 * Individual descriptor instances will be the same instances as those in the <code>properties</code> list.
	 */
	protected Map<String, DynaProperty> propertiesMap = new HashMap<>();

	/**
	 * Limits the size of the returned list. 
	 * The call to <code>getRows()</code> will return at most limit number of rows. 
	 * If less than or equal to 0, does not limit the size of the result.
	 */
	protected transient int limit = -1;

	private transient Document document;

	/**
	 * The list of {@link DynaBean}s representing the contents of the original 
	 * <code>ResultSet</code> on which this {@link RowSetDynaClass} was based.
	 */
	protected transient List<DynaBean> rows = new ArrayList<>();

	SQLRowSetDynaClass(Customer customer, Module module, String documentName, ResultSet resultSet)
	throws SQLException, MetaDataException {
		if (documentName != null) {
			document = module.getDocument(customer, documentName);
		}

		introspect(resultSet);
		copy(resultSet);
	}

	protected void introspect(ResultSet resultSet)
	throws SQLException {
		// Accumulate an ordered list of DynaProperties
		ArrayList<DynaProperty> list = new ArrayList<>();
		ResultSetMetaData metadata = resultSet.getMetaData();
		int n = metadata.getColumnCount();
		for (int i = 1; i <= n; i++) { // JDBC is one-relative!
			DynaProperty dynaProperty = null;
			if (metadata.getColumnName(i).toLowerCase().equals(PersistentBean.LOCK_NAME)) {
				dynaProperty = new DynaProperty(PersistentBean.LOCK_NAME, OptimisticLock.class);
			}
			else {
				dynaProperty = createDynaProperty(metadata, i);
			}

			if (dynaProperty != null) {
				list.add(dynaProperty);
			}
		}

		// Add the implicit properties document name and linkage document name
		list.add(new DynaProperty(Bean.MODULE_KEY, String.class));
		list.add(new DynaProperty(Bean.DOCUMENT_KEY, String.class));

		// Convert this list into the internal data structures we need
		properties = list.toArray(new DynaProperty[list.size()]);
		for (int i = 0; i < properties.length; i++) {
			propertiesMap.put(properties[i].getName(), properties[i]);
		}
	}

	protected void copy(ResultSet resultSet)
	throws SQLException {
		int cnt = 0;
		while (resultSet.next() && (limit < 0 || cnt++ < limit)) {
			DynaBean bean = new BasicDynaBean(this);
			for (int i = 0; i < properties.length; i++) {
				String name = properties[i].getName();
				if (name.equals("lockingdata")) {
					String lockingDataString = null;
					try {
						lockingDataString = resultSet.getString(name);
						bean.set(name, new OptimisticLock(lockingDataString));
					}
					catch (DomainException e) {
						throw new IllegalStateException("Could not parse Optimistic Lock " + lockingDataString, e);
					}
				}
				else if ((! name.equals(Bean.DOCUMENT_KEY)) && (! name.equals(Bean.MODULE_KEY))) {
					try {
						BindUtil.convertAndSet(bean, name, resultSet.getObject(name));
					}
					catch (Exception e) {
						throw new IllegalStateException("Could not convert and set value", e);
					}
				}
			}
			if (document != null) {
				bean.set(Bean.MODULE_KEY, document.getOwningModuleName());
				bean.set(Bean.DOCUMENT_KEY, document.getName());
			}

			rows.add(bean);
		}
	}

	@Override
	public String getName() {
		return getClass().getName();
	}

	@Override
	public DynaProperty getDynaProperty(String name) {
		if (name == null) {
			throw new IllegalArgumentException("No property name specified");
		}
		return propertiesMap.get(name);
	}

	@Override
	public DynaProperty[] getDynaProperties() {
		return properties;
	}

	@Override
	public DynaBean newInstance()
	throws IllegalAccessException, InstantiationException {
		throw new UnsupportedOperationException("newInstance() not supported");
	}

	/**
	 * <p>
	 * Factory method to create a new DynaProperty for the given index into the result set metadata.
	 * </p>
	 * 
	 * @param metadata is the result set metadata
	 * @param i is the column index in the metadata
	 * @return the newly created DynaProperty instance
	 */
	protected DynaProperty createDynaProperty(ResultSetMetaData metadata, int i)
	throws SQLException {
		Class<?> type = Object.class;

		String name = metadata.getColumnName(i).toLowerCase();

		Attribute attribute = ((document != null) ? document.getAttribute(name) : null);
		if (attribute != null) {
			type = attribute.getAttributeType().getImplementingType();
		}
		else {
			String className = null;
			try {
				className = metadata.getColumnClassName(i);
			}
			catch (SQLException e) {
				// this is a patch for HsqlDb to ignore exceptions thrown by its metadata implementation
			}

			if (className != null) {
				type = loadClass(className);
			}
		}

		return new DynaProperty(name, type);
	}

	/**
	 * Return a <code>List</code> containing the {@link DynaBean}s that represent 
	 * the contents of each <code>Row</code> from the <code>ResultSet</code> that was the basis 
	 * of this {@link RowSetDynaClass} instance. 
	 * These {@link DynaBean}s are disconnected from the database itself, 
	 * so there is no problem with modifying the contents of the list, 
	 * or the values of the properties of these {@link DynaBean}s. 
	 * However, it is the application's responsibility to persist 
	 * any such changes back to the database, if it so desires.
	 */
	public List<DynaBean> getRows() {
		return rows;
	}

	/**
	 * Loads and returns the <code>Class</code> of the given name. 
	 * By default, a load from the thread context class loader is attempted. 
	 * If there is no such class loader, the class loader used to load this class will be utilised.
	 * 
	 * @exception SQLException if an exception was thrown trying to load the specified class
	 */
	protected Class<?> loadClass(String className) throws SQLException {
		try {
			ClassLoader cl = Thread.currentThread().getContextClassLoader();
			if (cl == null) {
				cl = this.getClass().getClassLoader();
			}
			return (cl.loadClass(className));
		}
		catch (Exception e) {
			throw new SQLException("Cannot load column class '" + className + "': " + e);
		}
	}
}
