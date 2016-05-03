package modules.admin.Configuration;

import modules.admin.domain.Configuration;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

public class ConfigurationBizlet extends Bizlet<Configuration> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1282437688681930236L;
	
	@Override
	public Configuration newInstance(Configuration bean) throws Exception {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		Configuration result = q.beanResult();
		if (result == null) {
			result = bean;
		}

		if (result.getPasswordComplexityModel() == null) {
			result.setPasswordComplexityModel(ComplexityModel.DEFAULT_COMPLEXITY_MODEL);
		}

		return result;
	}
	
	
}
