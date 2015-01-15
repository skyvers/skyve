package org.skyve.wildcat.tools.jasperreports;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.jasperreports.WildcatDataSource;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.HibernateJackrabbitPersistence;

public class WildcatQueryExecuter implements JRQueryExecuter {
	private String moduleDotQuery;
	
	public WildcatQueryExecuter(String queryString) {
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
			Query query = getQuery(moduleDotQuery);
			DocumentQuery documentQuery = query.constructDocumentQuery(null, null);
			Persistence persistence = CORE.getPersistence();
			return new WildcatDataSource(persistence.getUser(),
											persistence.iterate(documentQuery).iterator());
        }
        catch (Exception e) {
        	throw new JRException("Could not create a BizHubQueryDataSource for " + moduleDotQuery, e);
        }
	}
	
	public static Query getQuery(String moduleDotQuery)
	throws MetaDataException {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateJackrabbitPersistence.class;
		UserImpl user = new UserImpl();
		user.setCustomerName("bizhub");
		user.setName("mike");
		AbstractPersistence.get().setUser(user);

		int dotIndex = moduleDotQuery.indexOf('.');
		Customer customer = AbstractPersistence.get().getUser().getCustomer();
		Module module = customer.getModule(moduleDotQuery.substring(0, dotIndex));
		String queryName = moduleDotQuery.substring(dotIndex + 1);
		Query query = module.getQuery(queryName);
		if (query == null) {
    		query = module.getDocumentDefaultQuery(customer, queryName);
		}

		return query;
	}
	
	public static void main(String[] args)
	throws Exception {
		new WildcatQueryExecuter("admin.Contact").createDatasource();
	}
}
