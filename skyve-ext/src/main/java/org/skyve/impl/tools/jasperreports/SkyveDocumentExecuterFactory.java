package org.skyve.impl.tools.jasperreports;

import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.ProvidedRepository;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JRValueParameter;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.JRQueryExecuter;
import net.sf.jasperreports.engine.query.QueryExecuterFactory;

/**
 * Factory that creates Jasper query executers for Skyve document query language.
 */
public class SkyveDocumentExecuterFactory implements QueryExecuterFactory {
	/**
	 * Creates a Skyve document query executer for the dataset query text.
	 *
	 * @param jasperReportsContext The Jasper reports context.
	 * @param dataset The report dataset containing query metadata.
	 * @param parameters The query parameter map.
	 * @return A query executer when a dataset query is present, otherwise {@code null}.
	 * @throws JRException If executer creation fails.
	 */
	@Override
	public JRQueryExecuter createQueryExecuter(JasperReportsContext jasperReportsContext, JRDataset dataset, Map<String, ? extends JRValueParameter> parameters)
	throws JRException {
		if (dataset != null) {
			JRQuery query = dataset.getQuery();
			if (query != null) {
				return new SkyveDocumentExecuter(query.getText());
			}
		}
		
		return null;
	}

	/**
	 * Returns Jasper built-in parameters supported by this factory.
	 *
	 * @return An empty array, as no additional built-in parameters are defined.
	 */
	@Override
	public Object[] getBuiltinParameters() {
		return new Object[0];
	}

	/**
	 * Indicates whether this factory supports the supplied query parameter type.
	 *
	 * @param className The query parameter class name.
	 * @return {@code true}, as all parameter types are accepted.
	 */
	@Override
	public boolean supportsQueryParameterType(String className) {
		return true;
	}
	
	/**
	 * Lazy singleton holder for the design-time user used by Jasper query execution.
	 */
	private static class UserSingleton {
		/**
		 * Singleton user instance initialised for report-design execution contexts.
		 */
		private static UserImpl user;
		static {
			ProvidedRepository repository = new LocalDesignRepository() {
				// Don't use bizlets for reporting
				@Override
				public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document, boolean runtime) {
					return null;
				}
			};
			ProvidedRepositoryFactory.set(repository);
			
			user = new SuperUser();
			user.setCustomerName("bizhub");
			user.setName("user");
			
		}
	}

	/**
	 * Returns the singleton design-time user used by Jasper tooling integration.
	 *
	 * @return The singleton design-time user.
	 */
	public static org.skyve.metadata.user.User getUser() {
		return UserSingleton.user;
	}
}
