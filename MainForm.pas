unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, InternetStatusThread, IdSSLOpenSSL, IdHTTP,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TForm1 = class(TForm)
    lblStatus: TLabel;
    btnStart: TButton;
    btnStop: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInternetThread: TInternetStatusThread;
    procedure HandleStatusChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  lblStatus.Caption := 'Нажмите "Старт" для начала мониторинга';
  btnStop.Enabled := False;

  self.btnStart.Click;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FInternetThread) then
  begin
    FInternetThread.Terminate;
    FInternetThread.WaitFor;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if not Assigned(FInternetThread) then
  begin
    FInternetThread := TInternetStatusThread.Create(True); // Создаем приостановленным
    FInternetThread.FreeOnTerminate := False;
    FInternetThread.OnStatusChange := HandleStatusChange;
    FInternetThread.Start;

    btnStart.Enabled := False;
    btnStop.Enabled := True;
    lblStatus.Caption := 'Мониторинг запущен...';
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  if Assigned(FInternetThread) then
  begin
    FInternetThread.Terminate;
    FInternetThread.WaitFor;
    FreeAndNil(FInternetThread);

    btnStart.Enabled := True;
    btnStop.Enabled := False;
    lblStatus.Caption := 'Мониторинг остановлен';
  end;
end;

procedure TForm1.HandleStatusChange(Sender: TObject);
begin
  lblStatus.Caption := FInternetThread.LastStatus;

  // Дополнительная визуализация
  if Pos('доступен', FInternetThread.LastStatus) > 0 then
    lblStatus.Font.Color := clGreen
  else if Pos('отсутствует', FInternetThread.LastStatus) > 0 then
    lblStatus.Font.Color := clRed
  else
    lblStatus.Font.Color := clMaroon;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Обновляем время для демонстрации работы приложения
  Caption := 'Монитор интернета - ' + FormatDateTime('hh:nn:ss', Now);
end;

end.
