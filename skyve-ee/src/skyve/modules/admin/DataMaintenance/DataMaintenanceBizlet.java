package modules.admin.DataMaintenance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.ModulesUtil.DomainValueSortByDescription;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenanceModuleDocument;

public class DataMaintenanceBizlet extends Bizlet<DataMaintenance> {
	private static final long serialVersionUID = 1L;
	
	public static final String SYSTEM_DATA_REFRESH_NOTIFICATION = "SYSTEM Document Data Refresh Notification";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_SUBJECT = "Perform Document Data Refresh - Complete";
	public static final String SYSTEM_DATA_REFRESH_DEFAULT_BODY = "The document data refresh is complete. Check Job log for details.";


	@Override
	public DataMaintenance newInstance(DataMaintenance bean) throws Exception {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery q = persistence.newDocumentQuery(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		DataMaintenance result = q.beanResult();
		if (result == null) {
			result = bean;
		}
		
		Customer c = CORE.getUser().getCustomer();
		for (Module m : c.getModules()) {
			for (String k : m.getDocumentRefs().keySet()) {
				Document d = m.getDocument(c, k);
				if (d.getPersistent() != null) {
					DataMaintenanceModuleDocument doc = DataMaintenanceModuleDocument.newInstance();
					doc.setModuleName(m.getName());
					doc.setDocumentName(d.getName());
					doc.setModDocName(String.format("%s.%s", m.getTitle(), d.getSingularAlias()));
					result.getRefreshDocuments().add(doc);
				}
			}
		}

		return result;
	}


	@Override
	public List<org.skyve.metadata.model.document.Bizlet.DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		List<DomainValue> result = new ArrayList<>();
		
		if (DataMaintenance.modDocNamePropertyName.equals(attributeName) ) {

			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				for (String k : m.getDocumentRefs().keySet()) {
					Document d = m.getDocument(c, k);
					if (d.getPersistent() != null) {
						result.add(new DomainValue(String.format("%s.%s", m.getName(), k), 
													String.format("%s.%s", m.getTitle(), d.getSingularAlias())));
					}
				}
			}
			Collections.sort(result, new DomainValueSortByDescription());
		}
		
		if(DataMaintenance.auditModuleNamePropertyName.equals(attributeName)){
			Customer c = CORE.getUser().getCustomer();
			for (Module m : c.getModules()) {
				result.add(new DomainValue(m.getName(), m.getTitle()));
			}
			Collections.sort(result, new DomainValueSortByDescription());			
		}

		return result;
	}


	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, DataMaintenance bean)
			throws Exception {

		
		if(DataMaintenance.auditDocumentNamePropertyName.equals(attributeName) && bean.getAuditModuleName()!=null){
			List<DomainValue> result = new ArrayList<>();
			Customer c = CORE.getUser().getCustomer();
			Module m = c.getModule(bean.getAuditModuleName());
			for (String k : m.getDocumentRefs().keySet()) {
				Document d = m.getDocument(c, k);
				if (d.getPersistent() != null) {
					result.add(new DomainValue(d.getName(), d.getSingularAlias()));
				}
			}

			Collections.sort(result, new DomainValueSortByDescription());
			return result;
		}
		
		return super.getDynamicDomainValues(attributeName, bean);
	}



}
