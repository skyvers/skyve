package org.skyve.wildcat.dataaccess.sql;

import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.Time;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.ProjectedQuery;
import org.skyve.util.Binder;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.persistence.AbstractQuery;
import org.skyve.wildcat.persistence.AbstractSQL;

import com.vividsolutions.jts.geom.Geometry;

public class SQL extends AbstractSQL implements ProjectedQuery {
	private SQLDataAccess dataAccess;
	private Document document;
	
	SQL(Document document, String query, SQLDataAccess dataAccess) {
		super(document, query);
		this.document = document;
		this.dataAccess = dataAccess;
	}

	SQL(String moduleName, String documentName, String query, SQLDataAccess dataAccess)
	throws MetaDataException {
		super(moduleName, documentName, query);
		Customer customer = CORE.getUser().getCustomer();
		this.document = customer.getModule(moduleName).getDocument(customer, documentName);
		this.dataAccess = dataAccess;
	}

	SQL(String query, SQLDataAccess dataAccess) {
		super(query);
		this.dataAccess = dataAccess;
	}

	@Override
	public <T extends Bean> List<T> beanResults() throws DomainException {
		if (document == null) {
			throw new DomainException("The document must be set to create beans from SQL");
		}
		
		List<T> results = new ArrayList<>(100);
		try {
			User user = CORE.getUser();
			Customer customer = user.getCustomer();
			
			// Collect all attributes for a document inheritance hierarchy
			List<Attribute> attributes = new ArrayList<>(document.getAttributes());
			Extends inherits = document.getExtends();
			while (inherits != null) {
				Module module = customer.getModule(document.getOwningModuleName());
				Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
				attributes.addAll(baseDocument.getAttributes());
				inherits = baseDocument.getExtends();
			}

			@SuppressWarnings("resource")
			Connection connection = dataAccess.getConnection();

			try (NamedParameterPreparedStatement ps = new NamedParameterPreparedStatement(connection, toQueryString())) {
				for (String parameterName : getParameterNames()) {
					ps.setObject(parameterName, getParameter(parameterName));
				}

				try (ResultSet rs = ps.executeQuery()) {
					while (rs.next()) {
						T bean = document.newInstance(user);
	
						for (Attribute attribute : attributes) {
							String name = attribute.getName();
							AttributeType type = attribute.getAttributeType();
							if (AttributeType.bool.equals(type)) {
								boolean value = rs.getBoolean(name);
								Binder.set(bean, name, rs.wasNull() ? null : Boolean.valueOf(value));
							}
							else if (AttributeType.colour.equals(type) ||
										AttributeType.content.equals(type) ||
										AttributeType.enumeration.equals(type) ||
										AttributeType.markup.equals(type) ||
										AttributeType.memo.equals(type) ||
										AttributeType.text.equals(type)) {
								Binder.convertAndSet(bean, name, rs.getString(name));
							}
							else if (AttributeType.date.equals(type)) {
								Date value = rs.getDate(name);
								if (value != null) {
									Binder.convertAndSet(bean, name, value);
								}
							}
							else if (AttributeType.dateTime.equals(type)) {
								Time value = rs.getTime(name);
								if (value != null) {
									Binder.convertAndSet(bean, name, value);
								}
							}
							else if (AttributeType.decimal10.equals(type)) {
								double value = rs.getDouble(name);
								Binder.set(bean, name, rs.wasNull() ? null : new Decimal10(value));
							}
							else if (AttributeType.decimal2.equals(type)) {
								double value = rs.getDouble(name);
								Binder.set(bean, name, rs.wasNull() ? null : new Decimal2(value));
							}
							else if (AttributeType.decimal5.equals(type)) {
								double value = rs.getDouble(name);
								Binder.set(bean, name, rs.wasNull() ? null : new Decimal5(value));
							}
							else if (AttributeType.geometry.equals(type)) {
								Geometry geometry = (Geometry) dataAccess.getGeometryUserType().nullSafeGet(rs, new String[] {name}, null);
								Binder.set(bean, name, geometry);
							}
							else if (AttributeType.integer.equals(type)) {
								int value = rs.getInt(name);
								Binder.set(bean, name, rs.wasNull() ? null : Integer.valueOf(value));
							}
							else if (AttributeType.longInteger.equals(type)) {
								long value = rs.getLong(name);
								Binder.set(bean, name, rs.wasNull() ? null : Long.valueOf(value));
							}
							else if (AttributeType.time.equals(type)) {
								Time value = rs.getTime(name);
								if (value != null) {
									Binder.convertAndSet(bean, name, value);
								}
							}
							else if (AttributeType.timestamp.equals(type)) {
								Date value = rs.getDate(name);
								if (value != null) {
									Binder.convertAndSet(bean, name, value);
								}
							}
						}
	
						results.add(bean);
					}
				}
			}
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}

		return results;
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable()
	throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T extends Bean> List<T> projectedResults() throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public final <T extends Bean> T projectedResult() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() throws DomainException {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable()
	throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) throws DomainException {
		List<T> results = new ArrayList<>(100);
		try {
			@SuppressWarnings("resource")
			Connection connection = dataAccess.getConnection();

			try (NamedParameterPreparedStatement ps = new NamedParameterPreparedStatement(connection, toQueryString())) {
				for (String parameterName : getParameterNames()) {
					ps.setObject(parameterName, getParameter(parameterName));
				}

				try (ResultSet rs = ps.executeQuery()) {
					while (rs.next()) {
						@SuppressWarnings("unchecked")
						T result = (T) BindUtil.convert(type, rs.getObject(1));
						results.add(result);
					}
				}
			}
		}
		catch (Throwable t) {
			throw new DomainException(t);
		}

		return results;
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type)
	throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Object[]> tupleResults() throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() throws DomainException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int execute() throws DomainException {
		// TODO Auto-generated method stub
		return 0;
	}
}
