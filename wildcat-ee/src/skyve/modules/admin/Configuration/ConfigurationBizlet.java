package modules.admin.Configuration;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.AdminUtil;
import modules.admin.domain.Configuration;

public class ConfigurationBizlet extends Bizlet<Configuration> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1282437688681930236L;

	@Override
	public Configuration newInstance(Configuration bean) throws Exception {
		
		Configuration result = bean;
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		List<Configuration> parameters = q.beanResults();
		if (parameters.size() > 0) {
			result = parameters.get(0);
		}

		if (result.getPasswordComplexityModel() == null) {
			result.setPasswordComplexityModel(AdminUtil.defaultPasswordComplexityModel);
		}

		return result;
	}
}
