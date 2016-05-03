package org.skyve.impl.tools.jasperreports;

import java.util.Map;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.query.JRQueryExecuter;
import net.sf.jasperreports.engine.query.JRQueryExecuterFactory;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;

@SuppressWarnings("deprecation")
public class SkyveDocumentExecuterFactory implements JRQueryExecuterFactory {
	@Override
	@SuppressWarnings("rawtypes")
	public JRQueryExecuter createQueryExecuter(JRDataset dataset, Map parameters) 
	throws JRException {
		if (dataset != null) {
			JRQuery query = dataset.getQuery();
			if (query != null) {
				return new SkyveDocumentExecuter(query.getText());
			}
		}
		
		return null;
	}

	@Override
	public Object[] getBuiltinParameters() {
		return new Object[0];
	}

	@Override
	public boolean supportsQueryParameterType(String className) {
		return true;
	}
	
	private static UserImpl user;
	public static org.skyve.metadata.user.User getUser() {
		if (user == null) {
			AbstractRepository repository = new LocalDesignRepository() {
				// Don't use bizlets for reporting
				@Override
				public <T extends Bean> Bizlet<T> getBizlet(Customer customer, Document document)
				throws MetaDataException {
					return null;
				}
			};
			AbstractRepository.set(repository);
			
			user = new SuperUser();
			user.setCustomerName("bizhub");
			user.setName("user");
		}
		
		return user;
	}
}
