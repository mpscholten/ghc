module AesonTypes where

-- Plain data types that will have JSON instances derived via TH in AesonBench

data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userActive :: Bool
  , userScore :: Double
  } deriving (Show, Eq)

data Address = Address
  { addrStreet :: String
  , addrCity :: String
  , addrState :: String
  , addrZip :: String
  , addrCountry :: String
  } deriving (Show, Eq)

data Product = Product
  { prodName :: String
  , prodPrice :: Double
  , prodQuantity :: Int
  , prodCategory :: String
  , prodDescription :: String
  , prodSku :: String
  } deriving (Show, Eq)

data Order = Order
  { orderId :: Int
  , orderUserId :: Int
  , orderTotal :: Double
  , orderStatus :: String
  , orderDate :: String
  , orderItems :: [OrderItem]
  } deriving (Show, Eq)

data OrderItem = OrderItem
  { itemProductId :: Int
  , itemQuantity :: Int
  , itemPrice :: Double
  , itemDiscount :: Double
  } deriving (Show, Eq)

data Company = Company
  { companyName :: String
  , companyFounded :: Int
  , companyEmployees :: Int
  , companyRevenue :: Double
  , companyPublic :: Bool
  , companyCeo :: String
  } deriving (Show, Eq)

data Employee = Employee
  { empFirstName :: String
  , empLastName :: String
  , empDepartment :: String
  , empSalary :: Double
  , empHireDate :: String
  , empManager :: Maybe String
  , empTitle :: String
  } deriving (Show, Eq)

data Invoice = Invoice
  { invNumber :: String
  , invDate :: String
  , invDueDate :: String
  , invAmount :: Double
  , invTax :: Double
  , invPaid :: Bool
  , invNotes :: String
  } deriving (Show, Eq)

data Config = Config
  { cfgHost :: String
  , cfgPort :: Int
  , cfgDebug :: Bool
  , cfgMaxRetries :: Int
  , cfgTimeout :: Double
  , cfgLogLevel :: String
  } deriving (Show, Eq)

data Event = Event
  { evtName :: String
  , evtTimestamp :: String
  , evtPayload :: String
  , evtSource :: String
  , evtSeverity :: Int
  } deriving (Show, Eq)

data ApiResponse = ApiResponse
  { respStatus :: Int
  , respMessage :: String
  , respData :: String
  , respTimestamp :: String
  , respRequestId :: String
  } deriving (Show, Eq)

data Session = Session
  { sessId :: String
  , sessUserId :: Int
  , sessToken :: String
  , sessCreated :: String
  , sessExpires :: String
  , sessActive :: Bool
  } deriving (Show, Eq)

data Notification = Notification
  { notifId :: Int
  , notifUserId :: Int
  , notifTitle :: String
  , notifBody :: String
  , notifRead :: Bool
  , notifCreated :: String
  } deriving (Show, Eq)

data Permission = Permission
  { permRole :: String
  , permResource :: String
  , permAction :: String
  , permGranted :: Bool
  } deriving (Show, Eq)

data AuditLog = AuditLog
  { auditAction :: String
  , auditUserId :: Int
  , auditTimestamp :: String
  , auditDetails :: String
  , auditIpAddress :: String
  } deriving (Show, Eq)

data Metric = Metric
  { metricName :: String
  , metricValue :: Double
  , metricUnit :: String
  , metricTimestamp :: String
  , metricTags :: [String]
  } deriving (Show, Eq)

data Report = Report
  { reportTitle :: String
  , reportAuthor :: String
  , reportDate :: String
  , reportSummary :: String
  , reportStatus :: String
  , reportPages :: Int
  } deriving (Show, Eq)

data Task = Task
  { taskTitle :: String
  , taskDescription :: String
  , taskAssignee :: String
  , taskPriority :: Int
  , taskDue :: String
  , taskCompleted :: Bool
  } deriving (Show, Eq)

data Comment = Comment
  { commentAuthor :: String
  , commentBody :: String
  , commentTimestamp :: String
  , commentLikes :: Int
  , commentEdited :: Bool
  } deriving (Show, Eq)

data Tag = Tag
  { tagName :: String
  , tagColor :: String
  , tagCategory :: String
  } deriving (Show, Eq)

data Webhook = Webhook
  { whUrl :: String
  , whSecret :: String
  , whEvents :: [String]
  , whActive :: Bool
  , whRetries :: Int
  } deriving (Show, Eq)
