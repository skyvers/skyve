package modules.whosin.Position;

import modules.whosin.domain.Position;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

public class PositionBizlet extends Bizlet<Position> {

	@Override
	public Position preExecute(ImplicitActionName actionName, Position bean, Bean parentBean, WebContext webContext) throws Exception {
		
		if(ImplicitActionName.Save.equals(actionName)){
			//keep the position in sync with the role title for the staff
			if(bean.originalValues().containsKey(Position.positionTitlePropertyName)){
				bean.getStaff().setRoleTitle(bean.getPositionTitle());
			}
		}
		
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 441801393959532437L;

	@Override
	public void preSave(Position bean) throws Exception {

		if (bean.getReportsTo() == null) {
			bean.setBizParentId(null);
		} else {
			bean.setBizParentId(bean.getReportsTo().getBizId());
		}

		super.preSave(bean);
	}

	@Override
	public void postLoad(Position bean) throws Exception {
		if (bean.getParent() != null) {
			bean.setReportsTo(bean.getParent());
		}
		super.postLoad(bean);
	}

}
