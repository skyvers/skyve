package modules.whosin.Position;

import org.skyve.metadata.model.document.Bizlet;

import modules.whosin.domain.Position;

public class PositionBizlet extends Bizlet<Position> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 441801393959532437L;

	/**
	 * Load the transient reportsTo (to allow switching within the hierarchy
	 */
	@Override
	public void postLoad(Position bean) throws Exception {
		if (bean.getParent() != null) {
			bean.setReportsTo(bean.getParent());
		}
		super.postLoad(bean);
	}

	/**
	 * When position is saved, update the position in the organisational structure hierarchy
	 */
	@Override
	public void preSave(Position bean) throws Exception {

		if (bean.getReportsTo() == null) {
			bean.setBizParentId(null);
		} else {
			bean.setBizParentId(bean.getReportsTo().getBizId());
		}

		super.preSave(bean);
	}


}
