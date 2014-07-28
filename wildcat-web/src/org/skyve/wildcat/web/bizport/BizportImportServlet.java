package org.skyve.wildcat.web.bizport;

import java.io.File;
import java.io.FileInputStream;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortException.Problem;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.bizport.POIWorkbook;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.JSONUtil;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.upload.AbstractUploadServlet;

public class BizportImportServlet extends AbstractUploadServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public StringBuilder processFile(File file, HttpServletRequest request, Map<String, Object> requestParameters)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		AbstractRepository repository = AbstractRepository.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();

		HttpSession session = request.getSession();
		String documentName = (String) session.getAttribute(AbstractUploadServlet.MODULE_DOT_DOCUMENT_NAME);
		int dotIndex = documentName.indexOf('.');
		String moduleName = documentName.substring(0, dotIndex);
		documentName = documentName.substring(dotIndex + 1);
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		BizImportAction bizPortAction = repository.getBizImportAction(customer, 
																		document, 
																		(String) session.getAttribute(BIZPORT_NAME));

		StringBuilder response = new StringBuilder(512);
		response.append("<html>");
		response.append("<head>");
		response.append("<meta name=\"robots\" content=\"noindex\">");
		response.append("<meta http-equiv=\"expires\" content=\"0\">");
		response.append("<meta http-equiv=\"pragma\" content=\"no-cache\"></head><body>");

		// smart client compressed includes
		response.append("<script type=\"text/javascript\">var isomorphicDir='").append(UtilImpl.SMART_CLIENT_DIR).append("/';</script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Core.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Foundation.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Containers.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Grids.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Forms.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_DataBinding.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_Calendar.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/system/modules/ISC_RichTextEditor.js\"></script>");
		response.append("<script type=\"text/javascript\" src=\"").append(UtilImpl.SMART_CLIENT_DIR).append("/skins/wildcat/load_skin.js\"></script>");
		
		response.append("<script type=\"text/javascript\">");

		response.append("isc.ListGrid.create({width:'100%',height:'100%',alternateRecordStyles:true,");
		response.append("wrapCells: true,fixedRecordHeights:false,data:[");

		BizPortException problems = new BizPortException();
		try {
			try (FileInputStream fis = new FileInputStream(file)) {
				POIWorkbook workbook = new POIWorkbook(persistence.getUser().getCustomer(),
														WorkbookFactory.create(fis), 
														problems);
				bizPortAction.bizImport(workbook, problems);
	
				
				// throw if we have errors found, to ensure rollback
				if (problems.hasErrors()) {
				      throw problems;
				}
	
				finishResponse(customer, problems, response);
			}
		}
		catch (BizPortException e) {
			e.printStackTrace();
			persistence.rollback();
			finishResponse(customer, problems, response);
		}
		finally {
			file.delete();
		}
		
		return response;
	}
	
	private static void finishResponse(Customer customer, 
										BizPortException problems, 
										StringBuilder response)
	throws Exception {
		for (Problem error : problems.getErrors()) {
			Map<String, Object> tuple = new TreeMap<>();
			tuple.put("where", error.getWhere());
			tuple.put("what", error.getWhat());
			tuple.put("severity", "error");
			response.append(JSONUtil.marshall(customer, tuple, null)).append(',');
		}
		for (Problem error : problems.getWarnings()) {
			Map<String, Object> tuple = new TreeMap<>();
			tuple.put("where", error.getWhere());
			tuple.put("what", error.getWhat());
			tuple.put("severity", "warning");
			response.append(JSONUtil.marshall(customer, tuple, null)).append(',');
		}

		if (problems.hasProblems()) {
			response.setLength(response.length() - 1); // remove last comma
		}

		response.append("],fields:[");
		response.append("{name:'severity',title:'Severity',width:60,align:'center',type:'image',imageURLPrefix:'../images/icons/',imageURLSuffix:'.png'},");
		response.append("{name:'where',title:'Cell',width:'33%'},");
		response.append("{name:'what',title:'Problem',width:'67%'}]});");

		response.append("window.top.document.getElementById('progress').src='blank.jsp';");

		if (problems.hasProblems()) {
			if (problems.hasErrors()) {
				response.append("isc.warn('The import did <b>NOT</b> complete successfully.<br/>" +
									"No data has changed as a result of this import.<br/>" +
									"Please review the errors and warnings displayed before closing this window.<br/>" + 
									"The above list includes only the first 50 errors and warnings, there may be more.<br/>" + 
									"If the nature of the problem is not clear from the message, it may be because it is caused by another issue being compounded.<br/>" + 
									"In this case, you may find that fixing one or two problems you can easily identify, may resolve a number of related issues.');");
			}
			else {
				response.append("isc.say('The import completed successfully with warnings.<br/>" +
									"Please review the warnings displayed before closing this window.<br/>" + 
									"The above list includes only the first 50 errors and warnings, there may be more.<br/>" + 
									"If the nature of the problem is not clear from the message, it may be because it is caused by another issue being compounded.<br/>" + 
									"In this case, you may find that fixing one or two problems you can easily identify, may resolve a number of related issues.');");
			}
		}
		else {
			response.append("isc.say('The import completed successfully.', function() {window.parent.close();});");
		}
		response.append("</script></body></html>");
	}
}
