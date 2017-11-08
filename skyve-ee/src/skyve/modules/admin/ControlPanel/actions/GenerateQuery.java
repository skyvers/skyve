package modules.admin.ControlPanel.actions;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.CORE;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class GenerateQuery implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = 5990074876826469688L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		if (bean.getDesignModuleDocumentName() != null) {
			try {
				Persistence persistence = CORE.getPersistence();
				User user = persistence.getUser();
				Customer customer = user.getCustomer();
	
				String[] modoc = bean.getDesignModuleDocumentName().split("\\.");
				Module module = customer.getModule(modoc[0]);
				Document document = module.getDocument(customer, modoc[1]);
	
				String queryName = "q" + document.getPluralAlias();
				StringBuilder sb = new StringBuilder();
	
				sb.append("<documentQuery name=\"").append(queryName).append("\"");
				sb.append(" documentName=\"").append(document.getName()).append("\">\n");
				sb.append("\t<displayName>").append(document.getSingularAlias()).append("</displayName>\n");
				if (document.getDescription() != null) {
					sb.append("\t<description>").append(document.getDescription()).append("</description>\n");
				}
				sb.append("\t<columns>\n");
	
				for (Attribute a : document.getAttributes()) {
					sb.append("\t\t<column>\n\t\t\t<binding>");
					sb.append(a.getName());
					if (AttributeType.association.equals(a.getAttributeType())) {
						sb.append(".bizKey");
					}
					sb.append("</binding>\n\t\t</column>\n");
				}
				sb.append("\t</columns>\n");
				sb.append("</documentQuery>");
	
				bean.setResults(sb.toString());
			}
			catch (Exception e) {
				bean.trapException(e);
			}
		}
		else {
			bean.setResults(null);
		}
		return new ServerSideActionResult<>(bean);
	}
}
