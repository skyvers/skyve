package org.skyve.impl.tools.jasperreports;

import org.skyve.CORE;
import org.skyve.impl.jasperreports.SkyveDataSource;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateElasticSearchPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

public class SkyveQueryExecuter implements JRQueryExecuter {
	private String moduleDotQuery;
	
	public SkyveQueryExecuter(String queryString) {
		moduleDotQuery = queryString;
	}
	
	@Override
	public boolean cancelQuery() 
	throws JRException {
// TODO - allow query to be cancelled
		return false;
	}

	@Override
	public void close() {
		// nothing to do here
	}

	@Override
	public JRDataSource createDatasource() 
	throws JRException {
		try {
			DocumentQueryDefinition query = getQuery(moduleDotQuery);
			DocumentQuery documentQuery = query.constructDocumentQuery(null, null);
			Persistence persistence = CORE.getPersistence();
			return new SkyveDataSource(persistence.getUser(),
											documentQuery.projectedIterable().iterator());
        }
        catch (Exception e) {
        	throw new JRException("Could not create a BizHubQueryDataSource for " + moduleDotQuery, e);
        }
	}
	
	public static DocumentQueryDefinition getQuery(String moduleDotQuery) {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
		UserImpl user = new UserImpl();
		user.setCustomerName("bizhub");
		user.setName("mike");
		AbstractPersistence.get().setUser(user);

		int dotIndex = moduleDotQuery.indexOf('.');
		Customer customer = AbstractPersistence.get().getUser().getCustomer();
		Module module = customer.getModule(moduleDotQuery.substring(0, dotIndex));
		String queryName = moduleDotQuery.substring(dotIndex + 1);
		DocumentQueryDefinition query = module.getDocumentQuery(queryName);
		if (query == null) {
    		query = module.getDocumentDefaultQuery(customer, queryName);
		}

		return query;
	}
	
	public static void main(String[] args)
	throws Exception {
		new SkyveQueryExecuter("admin.Contact").createDatasource();
	}
}
