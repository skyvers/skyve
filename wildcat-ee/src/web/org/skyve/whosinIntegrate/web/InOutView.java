package org.skyve.whosinIntegrate.web;

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.FacesContext;

import modules.admin.domain.Contact;
import modules.whosinIntegrate.domain.Office;
import modules.whosinIntegrate.domain.Staff;

import org.apache.commons.codec.binary.Base64;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebAction;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

@ViewScoped
@ManagedBean
public class InOutView extends FacesView<Office> {
	private static final long serialVersionUID = -6668236628273137985L;

	@Override
	public void preRender() {
		boolean postback = FacesContext.getCurrentInstance().isPostback();
		if (! postback) {
			// set the standard parameters ready for retrieval
			setBizModuleParameter(Office.MODULE_NAME);
			setBizDocumentParameter(Office.DOCUMENT_NAME);
			setWebActionParameter(WebAction.e);
		}
		
		// This loads the bean from the parameters and sets up ready for action
		super.preRender();
		
		if (! postback) {
			// Get all staff for this office
			staff = new FacesAction<List<Staff>>() {
				@Override
				@SuppressWarnings("synthetic-access")
				public List<Staff> callback()
				throws Exception {
					return retrieveStaff();
				}
			}.execute();
		}
	}

	private List<Staff> retrieveStaff()
	throws Exception {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
		q.getFilter().addEquals(Binder.createCompoundBinding(Staff.baseOfficePropertyName,
																Bean.DOCUMENT_ID),
									getBean().getBizId());
		q.addOrdering(Binder.createCompoundBinding(Staff.contactPropertyName, Contact.namePropertyName));
		return q.beanResults();
	}
	
	private List<Staff> staff = null;
	public List<Staff> getStaff() {
		return staff;
	}
	
	private Staff selectedStaff = null;
	public Staff getSelectedStaff() {
		return selectedStaff;
	}
	public void setSelectedStaff(Staff selectedStaff) {
		this.selectedStaff = selectedStaff;
	}

	private String base64Image = null;
	public String getBase64Image() {
		return base64Image;
	}
	public void setBase64Image(String base64Image) {
		this.base64Image = base64Image;
	}

	public String saveSelectedStaff() {
		return new FacesAction<String>() {
			@Override
			@SuppressWarnings("synthetic-access")
			public String callback() throws Exception {
				Persistence p = CORE.getPersistence();
				User u = p.getUser();
				
				if (base64Image != null) {
					// remove "data:image/png;base64," from the start
					int start = base64Image.indexOf(',') + 1;
					if (start > 0) {
						byte[] bytes = Base64.decodeBase64(base64Image.substring(start).getBytes());

						String bizCustomer = u.getCustomerName();
						Contact contact = selectedStaff.getContact();
						
						try (ContentManager cm = EXT.newContentManager()) {
							AttachmentContent content = new AttachmentContent(bizCustomer, 
																				Contact.MODULE_NAME, 
																				Contact.DOCUMENT_NAME,
																				u.getDataGroupId(), 
																				u.getId(),
																				contact.getBizId(), 
																				Contact.imagePropertyName,
																				MimeType.png,
																				bytes);
							content.setContentId(contact.getImage());
							cm.put(content);
							contact.setImage(content.getContentId());
							changedContactImageId = contact.getImage(); // the next render of graphic image will drop the cache
							base64Image = null;
						}
					}
				}

				p.save(selectedStaff);
				selectedStaff = null;
				staff = retrieveStaff();
				
				return "pm:inout";
			}
		}.execute();
	}
	
	// For refreshing images
	private String changedContactImageId;
	public boolean shouldCacheImage(String contactImageId) {
		String processedContactImageId = Util.processStringValue(contactImageId);
		if (processedContactImageId != null) {
			if (processedContactImageId.equals(changedContactImageId)) {
				changedContactImageId = null;
				return false;
			}
		}
		return true;
	}
}
