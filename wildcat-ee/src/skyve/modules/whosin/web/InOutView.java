package modules.whosin.web;

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.FacesContext;

import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebAction;
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
		return p.retrieve(q);
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
	
	public String saveSelectedStaff() {
		return new FacesAction<String>() {
			@Override
			@SuppressWarnings("synthetic-access")
			public String callback() throws Exception {
				CORE.getPersistence().save(selectedStaff);
				selectedStaff = null;
				staff = retrieveStaff();
				
				return "pm:inout";
			}
		}.execute();
	}
}
