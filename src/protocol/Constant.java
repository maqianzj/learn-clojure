package protocol;

/**
 * Э�鳣��
 * @author maqian
 * @version 1.0
 *
 */
public interface Constant {
	/*Э��汾��*/
	public static final byte PROTOCOL_VERSION = 1;
	
	/*Э���־*/
	public static final byte PROTOCOL_TAG_SYS_SERV = 1;		/*ϵͳ����Э��*/
	public static final byte PROTOCOL_TAG_RECORD = 2;		/*��Ϸ��¼Э��*/
	public static final byte PROTOCOL_TAG_ATTAINMENT = 3;	/*��Ϸ�ɾͣ����У�Э��*/
	public static final byte PROTOCOL_TAG_PROP = 4;			/*����Э��*/
	public static final byte PROTOCOL_TAG_SUBSCRIBE = 5;	/*����*/
	public static final byte PROTOCOL_TAG_PURCHASE = 6;		/*���Э��*/
	public static final byte PROTOCOL_TAG_ACCOUNT = 7;		/*�û��˻�Э��*/
	
	
	/*Э������*/
	public static final byte SYS_SERV_CMD_SYN_TIME = 1;				/*ʱ��ͬ��*/
	public static final byte SYS_SERV_CMD_ADD_FAVORITEGD = 2;		/*�㶫����ӵ��ղؼ�*/
	
	
	public static final byte RECORD_CMD_SAVE = 1;					/*�����¼*/
	public static final byte RECORD_CMD_READ = 2;					/*��ȡ��¼*/
	public static final byte RECORD_CMD_QUERY_DESC_LIST = 3;		/*��ѯ��¼�����б�*/
	public static final byte RECORD_CMD_UPDATE = 4;					/*���¼�¼*/
	
	public static final byte ATTAINMENT_CMD_SAVE = 1;				/*����ɾ�*/
	public static final byte ATTAINMENT_CMD_READ = 2;				/*��ȡ�ɾ�*/
	public static final byte ATTAINMENT_CMD_UPDATE = 3;				/*���³ɾ�*/
	public static final byte ATTAINMENT_CMD_QUERY_DESC_LIST = 4;	/*��ѯ�ɾ������б�*/
	public static final byte ATTAINMENT_CMD_QUERY_RANKING_LIST = 5;	/*��ѯ�����б�*/
	
	public static final byte PROP_CMD_QUERY_PROP_LIST = 1;			/*��ѯ�����б�*/
	public static final byte PROP_CMD_QUERY_OWN_PROP_LIST = 2;		/*��ѯӵ�е����б�*/ 
	public static final byte PROP_CMD_USE_PROPS = 3;					/*ʹ�õ���*/
	public static final byte PROP_CMD_SYN_PROPS = 4;					/*ͬ������*/
	
	public static final byte SUBSCRIBE_CMD_SUBSCRIBE = 1;				/*����*/
	public static final byte SUBSCRIBE_CMD_QUERY_SUBSCRIBE_RECORD = 2;	/*��ѯ������¼*/
	public static final byte SUBSCRIBE_CMD_RECHARGE = 3;				/*��ֵ*/
	public static final byte SUBSCRIBE_CMD_QUERY_BALANCE = 4;			/*��ѯ���*/
	public static final byte SUBSCRIBE_CMD_SUBSCRIBE_PRODUCT = 5;		/*������Ʒ*/
	public static final byte SUBSCRIBE_CMD_RECHARGE_WINSIDEGD = 6;		/*winsidegd��ֵ*/
	public static final byte SUBSCRIBE_CMD_GOTO_ORDER_PAGE_WINSIDEFJ = 7;		/*winsidefj��ת����ֵ����*/
	
	public static final byte PURCHASE_CMD_PURCHASE_PROP = 1;			/*�������*/
	public static final byte PURCHASE_CMD_EXPEND = 2;					/*���ѽ��*/
	public static final byte PURCHASE_CMD_QUERY_PURCHASE_RECORD = 3;	/*��ѯ��Ѽ�¼*/
	
	public static final byte ACCOUNT_CMD_QUERY_AUTHORIZATION = 1;		/*��ѯ��Ȩ��Ϣ*/
	public static final byte ACCOUNT_CMD_QUERY_SUB_PROPS = 2;			/*��ѯ�����������*/
	public static final byte ACCOUNT_CMD_USER_LOGIN = 3;				/*�û���¼*/
	
	
	/*������(Error Code)*/
	public static final byte EC_INVALID_TAG = -1;			/*Э���ʶ��Ч*/
	public static final byte EC_INVALID_CMD = -2;			/*Э��������Ч*/
	public static final byte EC_RECORD_NOT_EXIST = -3;		/*��Ϸ��¼������*/
	public static final byte EC_ATTAINMENT_NOT_EXIST = -4;	/*��Ϸ�ɾͲ�����*/
	
}
