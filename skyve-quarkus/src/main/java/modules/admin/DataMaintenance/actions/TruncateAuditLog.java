package modules.admin.DataMaintenance.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Audit;
import modules.admin.domain.DataMaintenance;

public class TruncateAuditLog implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(DataMaintenance.MODULE_NAME);
		JobMetaData job = module.getJob("jTruncateAuditLog");
		
		EXT.getJobScheduler().runOneShotJob(job, bean, user);
	
		bean.setAuditResponse("Job commenced.");
		

		return new ServerSideActionResult<>(bean);
	}
	
	/**
	 * Create the document query for the applicable filter selections
	 * 
	 * @param pers
	 * @param bean
	 * @return
	 * @throws Exception
	 */
	public static DocumentQuery getAuditQuery(Persistence pers, DataMaintenance bean) throws Exception {

		DocumentQuery qAudits = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		if (bean.getAuditModuleName() != null) {
			qAudits.getFilter().addEquals(Audit.auditModuleNamePropertyName, bean.getAuditModuleName());
		}
		if (bean.getAuditDocumentName() != null) {
			qAudits.getFilter().addEquals(Audit.auditDocumentNamePropertyName, bean.getAuditDocumentName());
		}
		if (bean.getAuditOperation() != null) {
			qAudits.getFilter().addEquals(Audit.operationPropertyName, bean.getAuditOperation());
		}
		if (bean.getAuditTimestampStart() != null) {
			qAudits.getFilter().addGreaterThanOrEqualTo(Audit.timestampPropertyName, bean.getAuditTimestampStart());
		}
		if (bean.getAuditTimestampEnd() != null) {
			qAudits.getFilter().addLessThanOrEqualTo(Audit.timestampPropertyName, bean.getAuditTimestampEnd());
		}
		if (bean.getAuditUser() != null) {
			qAudits.getFilter().addEquals(Audit.userNamePropertyName, bean.getAuditUser().getUserName());
		}

		return qAudits;
	}

	/**
	 * Get count of affected records
	 * 
	 * @param pers
	 * @param bean
	 * @return
	 * @throws Exception
	 */
	public static DataMaintenance setResultCount(Persistence pers, DataMaintenance bean) throws Exception {
		DataMaintenance result = bean;

		DocumentQuery qAudits = getAuditQuery(pers, bean);
		qAudits.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfAudits");

		Long countOfAudit = qAudits.scalarResult(Long.class);
		bean.setAuditMatchCount(Integer.valueOf(countOfAudit.intValue()));

		return result;
	}
}
