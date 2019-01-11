package modules.admin.ImportExport;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.actions.UploadSimpleImportDataFile;
import modules.admin.ImportExportColumn.ImportExportColumnBizlet;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;

public class ImportExportBizlet extends Bizlet<ImportExport> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3224886678815636057L;

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {

		//list of modules
		if (ImportExport.moduleNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getTitle()));
			}
			return result;
		}		
		
		return super.getConstantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ImportExport bean) throws Exception {

		//list documents within modules
		if (ImportExport.documentNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			List<DomainValue> result = new ArrayList<>();
			if (bean.getModuleName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
			}
			return result;
		}
		
		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public void preRerender(String source, ImportExport bean, WebContext webContext) throws Exception {
		
		updateColumns(source, bean);
		
		super.preRerender(source, bean, webContext);
	}
	
	public static void updateColumns(String source, ImportExport bean) throws Exception{
		switch (source) {
		case ImportExport.advancedModePropertyName:
			// switching into advanced mode, prepare binding expression equivalents
			if (Boolean.TRUE.equals(bean.getAdvancedMode())) {
				for (ImportExportColumn c : bean.getImportExportColumns()) {
					if (c.getBindingName() != null && !ImportExportColumnBizlet.ADVANCED.equals(c.getBindingName())) {
						c.setBindingExpression("{" + c.getBindingName() + "}");
					}
				}
			}
			break;
		case ImportExport.documentNamePropertyName:
			// if changing document name, recreate default import export column config
			bean.getImportExportColumns().clear();
		//$FALL-THROUGH$
	case ImportExport.modePropertyName:
			if(Mode.importData.equals(bean.getMode()) && bean.getImportFileAbsolutePath()!=null) {
				bean.getImportExportColumns().clear();
				UploadSimpleImportDataFile.loadColumnsFromFile(bean, new UploadException());				
			}
			if (Mode.exportData.equals(bean.getMode()) && bean.getImportExportColumns().size() == 0) {
				if (bean.getModuleName() != null && bean.getDocumentName() != null) {
					List<ImportExportColumn> columns = generateColumns(bean);
					for(ImportExportColumn c: columns) {
						c.setParent((ImportExportExtension) bean);
						bean.getImportExportColumns().add(c);
					}
				}
			}
			break;
		default:
			break;
	}

	}

	/**
	 * Generate column configs from scalar attributes
	 */
	public static List<ImportExportColumn> generateColumns(ImportExport bean) {
		
		List<ImportExportColumn> columns = new ArrayList<>();
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getModuleName());
		Document document = module.getDocument(customer, bean.getDocumentName());

		for (Attribute a : document.getAttributes()) {
			//exclude unsupported types
			if (!AttributeType.association.equals(a.getAttributeType())
					&& !AttributeType.collection.equals(a.getAttributeType())
					&& !AttributeType.content.equals(a.getAttributeType())
					&& !AttributeType.geometry.equals(a.getAttributeType())
					&& !AttributeType.inverseMany.equals(a.getAttributeType())
					&& !AttributeType.inverseOne.equals(a.getAttributeType())) {
				
				//also exclude non persistent fields
				if(a.isPersistent()) {
					ImportExportColumn col = ImportExportColumn.newInstance();
					col.setBindingName(a.getName());
					col.setColumnName(a.getDisplayName());
					columns.add(col);
				}
			}
		}
		
		return columns;
	}

}
