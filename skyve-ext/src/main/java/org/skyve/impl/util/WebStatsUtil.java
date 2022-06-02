package org.skyve.impl.util;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;
import org.skyve.web.UserAgentType;

public class WebStatsUtil {
	private static final String YEAR_FORMAT = "yyyy";
	private static final String MONTH_FORMAT = "M";

	private static final String WEB_STATS_MODULE_NAME = "admin";
	
	private WebStatsUtil() {
		// do nothing
	}
	
	public static void recordLogin(User user)
	throws Exception {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(WEB_STATS_MODULE_NAME);
		Document loginRecordDocument = module.getDocument(customer, "UserLoginRecord");
		AbstractPersistentBean loginRecord = loginRecordDocument.newInstance(user);
		BindUtil.set(loginRecord, "userName", user.getName());
		BindUtil.set(loginRecord, "loginDateTime", new DateTime(System.currentTimeMillis()));
		BindUtil.set(loginRecord, "failed", Boolean.FALSE);

		AbstractPersistence.get().save(loginRecordDocument, loginRecord);
// NO COMMIT
	}
	
	// this is called from the BizHubFilter - create and destroy a special persistence for this as
	// the actual persistence used for this thread is set in the servlets or the faces bean and 
	// could be different from the default created here for the thread - think webContext and conversations!!
	// If it was not closed {commit(true)} then it could create a resource leak
	public static synchronized void recordHit(User user, String userAgentHeader, UserAgentType userAgentType)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user); // user has not been set as the servlet for this filter have not fired yet
		persistence.begin();

		try {
			Customer customer = user.getCustomer();
			Module admin = customer.getModule(WEB_STATS_MODULE_NAME);
			@SuppressWarnings("null")
			String ADM_UserMonthlyHits = admin.getDocument(customer, "UserMonthlyHits").getPersistent().getPersistentIdentifier();

			Date now = new Date();
			Integer month = Integer.valueOf(CORE.getDateFormat(MONTH_FORMAT).format(now));
			Integer year = Integer.valueOf(CORE.getDateFormat(YEAR_FORMAT).format(now));
			
			String truncatedUserAgentHeader = userAgentHeader;
			if ((truncatedUserAgentHeader != null) && (truncatedUserAgentHeader.length() > 400)) {
				truncatedUserAgentHeader = truncatedUserAgentHeader.substring(0, 400);
			}
			String deviceCode = null;
			if (userAgentType == UserAgentType.desktop) {
				deviceCode = "D";
			}
			else if (userAgentType == UserAgentType.phone) {
				deviceCode = "P";
			}
			else if (userAgentType == UserAgentType.tablet) {
				deviceCode = "T";
			}
			else {
				deviceCode = "O";
			}
			
			StringBuilder query = new StringBuilder(128);
			query.append("select bizId from ").append(ADM_UserMonthlyHits);
			query.append(" where month = :month ");
			query.append("and year = :year ");
			query.append("and userName = :userName ");
			query.append("and device = :device ");
			if (userAgentHeader == null) {
				query.append("and userAgentHeader is null ");
			}
			else {
				query.append("and userAgentHeader = :userAgentHeader ");
			}
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				query.append("and bizCustomer = :customer");
			}
			SQL sql = persistence.newSQL(query.toString());
			sql.putParameter("month", month);
			sql.putParameter("year", year);
			sql.putParameter("userName", user.getName(), false);
			sql.putParameter("device", deviceCode, false);
			if (userAgentHeader != null) {
				sql.putParameter("userAgentHeader", truncatedUserAgentHeader, false);
			}
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				sql.putParameter("customer", user.getCustomer().getName(), false);
			}
			List<String> results = sql.scalarResults(String.class);
			if (results.isEmpty()) {
				query.setLength(0);
				query.append("insert into ").append(ADM_UserMonthlyHits);
				query.append(" (bizId, bizVersion, bizCustomer, bizLock, bizUserId, userName, year, month, userAgentHeader, device, numberOfHits, bizKey) values ");
				query.append("(:bizId, 0, :bizCustomer, :bizLock, :bizUserId, :userName, :year, :month, :userAgentHeader, :device, 1, :bizKey)");
				sql = persistence.newSQL(query.toString());
				sql.putParameter(Bean.DOCUMENT_ID, UUID.randomUUID().toString(), false);
				sql.putParameter(Bean.CUSTOMER_NAME, user.getCustomer().getName(), false);
				sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(user.getName(), now).toString(), false);
				sql.putParameter(Bean.USER_ID, user.getId(), false);
				sql.putParameter("userName", user.getName(), false);
				sql.putParameter("year", year);
				sql.putParameter("month", month);
				sql.putParameter("userAgentHeader", truncatedUserAgentHeader, false);
				sql.putParameter("device", deviceCode, false);
				sql.putParameter("bizKey", "bizKey", false);
				
				sql.execute();
			}
			else {
				query.setLength(0);
				query.append("update ").append(ADM_UserMonthlyHits);
				query.append(" set numberOfHits = numberOfHits + 1 ");
				query.append("where bizId = :bizId");
				sql = persistence.newSQL(query.toString());
				sql.putParameter(Bean.DOCUMENT_ID, results.get(0), false);
				sql.execute();
			}
		}
		catch (Exception e) {
			persistence.rollback();
			throw e;
		}
		finally {
			persistence.commit(true);
		}
	}
}
