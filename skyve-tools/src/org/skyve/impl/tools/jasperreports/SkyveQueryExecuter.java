package org.skyve.impl.tools.jasperreports;

import org.skyve.CORE;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

/**
 * Executes JasperReports queries using Skyve metadata queries.
 *
 * <p>Query text is interpreted as {@code module.query}. The executer creates a
 * projected Skyve {@link DocumentQuery} and exposes it through
 * {@link SkyveDataSource}.
 */
public class SkyveQueryExecuter implements JRQueryExecuter {
	private String moduleDotQuery;
	
	/**
	 * Creates an executer for a {@code module.query} identifier.
	 *
	 * @param queryString query selector in {@code module.query} format
	 */
	public SkyveQueryExecuter(String queryString) {
		moduleDotQuery = queryString;
	}
	
	/**
	 * Indicates cancellation is not currently supported.
	 *
	 * @return always {@code false}
	 * @throws JRException if cancellation fails
	 */
	@Override
	public boolean cancelQuery() 
	throws JRException {
// TODO - allow query to be cancelled
		return false;
	}

	/**
	 * Releases resources associated with this executer.
	 *
	 * <p>No explicit resources are held, so this method is a no-op.
	 */
	@Override
	public void close() {
		// nothing to do here
	}

	/**
	 * Creates a JasperReports data source backed by a Skyve projected query.
	 *
	 * @return data source iterating projected query rows
	 * @throws JRException if metadata lookup or query construction fails
	 */
	@Override
	@SuppressWarnings("resource")
	public JRDataSource createDatasource() 
	throws JRException {
		try {
			MetaDataQueryDefinition query = getQuery(moduleDotQuery);
			DocumentQuery documentQuery = query.constructDocumentQuery(null, null);
			Persistence persistence = CORE.getPersistence();
			return new SkyveDataSource(persistence.getUser(),
											documentQuery.projectedIterable().iterator());
        }
        catch (Exception e) {
        	throw new JRException("Could not create a BizHubQueryDataSource for " + moduleDotQuery, e);
        }
	}
	
	/**
	 * Resolves metadata query definition from {@code module.query} notation.
	 *
	 * @param moduleDotQuery query selector in {@code module.query} format
	 * @return resolved metadata query definition
	 */
	public static MetaDataQueryDefinition getQuery(String moduleDotQuery) {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
		UserImpl user = new UserImpl();
		user.setCustomerName("bizhub");
		user.setName("mike");
		AbstractPersistence.get().setUser(user);

		int dotIndex = moduleDotQuery.indexOf('.');
		Customer customer = AbstractPersistence.get().getUser().getCustomer();
		Module module = customer.getModule(moduleDotQuery.substring(0, dotIndex));
		String queryName = moduleDotQuery.substring(dotIndex + 1);
		MetaDataQueryDefinition query = module.getMetaDataQuery(queryName);
		if (query == null) {
    		query = module.getDocumentDefaultQuery(customer, queryName);
		}

		return query;
	}
	
	/**
	 * Runs a basic manual check for query data source creation.
	 *
	 * @param args not used
	 * @throws Exception if data-source creation fails
	 */
	public static void main(String[] args)
	throws Exception {
		new SkyveQueryExecuter("admin.Contact").createDatasource();
	}
}
