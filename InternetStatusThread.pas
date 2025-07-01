unit InternetStatusThread;

interface

uses
  Classes, SysUtils, WinInet, IdSSLOpenSSL, IdHTTP, IdComponent, IdTCPConnection, IdTCPClient;



type
  TNotifyEvent = procedure(Sender: TObject) of object;

  TInternetStatusThread = class(TThread)
  private
    FInterval: Integer;
    FLastStatus: string;
    FOnStatusChange: TNotifyEvent;
    procedure UpdateStatus;
  protected
    procedure Execute; override;
    function RebootRouter: boolean;
  public
    constructor Create(CreateSuspended: Boolean; CheckInterval: Integer = 5000);
    property LastStatus: string read FLastStatus;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  end;

implementation

function TInternetStatusThread.RebootRouter: Boolean;
var
  HTTP: TIdHTTP;
  Params: TStringList;
  Response: string;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result := False;
  HTTP := TIdHTTP.Create(nil);
  Params := TStringList.Create;
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    try
      // Настройка SSL при необходимости
      HTTP.IOHandler := SSLHandler;
      SSLHandler.SSLOptions.Method := sslvSSLv23;
      SSLHandler.SSLOptions.Mode := sslmClient;

      // Добавление параметров авторизации
      Params.Add('username=admin');
      Params.Add('password=admin');

      // Настройка HTTP-клиента
      HTTP.Request.ContentType := 'application/x-www-form-urlencoded';
      HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)';
      HTTP.HandleRedirects := True;

      // Отправка POST-запроса
      Response := HTTP.Post('http://192.168.1.1/login.html', Params);

      Params.Clear;
      Params.Add('isTest=false');
      Params.Add('goformId=REBOOT_DEVICE');

      Response := HTTP.Post('http://192.168.1.1/goform/goform_set_cmd_process', Params);
    except
      on E: Exception do
      begin
        FLastStatus := 'Ошибка перезагрузки роутера';
        Synchronize(UpdateStatus);
      end;
    end;
  finally
    HTTP.Free;
    Params.Free;
    SSLHandler.Free;
  end;
end;

{ TInternetStatusThread }

constructor TInternetStatusThread.Create(CreateSuspended: Boolean; CheckInterval: Integer);
begin
  inherited Create(CreateSuspended);
  FInterval := CheckInterval;
  FreeOnTerminate := False; // Управляем освобождением вручную
end;

procedure TInternetStatusThread.Execute;
const
  TEST_URL = 'http://www.google.com';
var
  NewStatus: string;
  TCP:TIdTCPClient;
  IsInternet: boolean;
begin
  while not Terminated do
  begin
    // Проверка физического подключения
     TCP:=TIdTCPClient.Create(nil);
      try
        TCP.Host:='google.com';
        TCP.Port:=80;
        TCP.ReadTimeout:=2000;
        try
          TCP.Connect;
          IsInternet:=TCP.Connected;

        except
          IsInternet:=false;
        end;
      finally
        TCP.Free;
      end;

    if not InternetGetConnectedState(nil, 0) then
      NewStatus := 'Сетевое подключение отсутствует'
    // Проверка доступа в интернет
    else if not IsInternet then
      begin
        FLastStatus := 'Перезагрузка роутера';

        Synchronize(UpdateStatus);

        RebootRouter;
        Sleep(60000);
      end
    else
      NewStatus := 'Интернет доступен';

    // Обновляем статус только при его изменении
    if NewStatus <> FLastStatus then
    begin
      FLastStatus := NewStatus;
      Synchronize(UpdateStatus);
    end;
    // Задержка между проверками
    Sleep(FInterval);
  end;
end;

procedure TInternetStatusThread.UpdateStatus;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

end.

